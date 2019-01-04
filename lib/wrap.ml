type t =
  { encoder : Encoder.encoder
  ; mutable scan_stack : scan_atom list
  (* the pretty-printer formatting stack. each element is (left_total, element)
     where left_total is the value of left_total when the element has been
     enqueued *)
  ; mutable format_stack : format_atom list
  (* the pretty-printer formatting stack. each stack element describes a
     pretty-printer box. *)
  ; margin : int
  ; newline : string
  (* value of the right margin *)
  ; mutable max_indent : int
  (* maximum value of indentation: no box can be opened further *)
  ; mutable space_left : int
  (* space remaining on the current line *)
  ; mutable min_space_left : int
  ; mutable current_indent : int
  (* current value of indentation *)
  ; mutable left_total : int
  (* total width of tokens already printed *)
  ; mutable right_total : int
  (* total width of tokens ever put in the queue *)
  ; mutable is_new_line : bool
  (* true when the line has been broken by the pretty-printer *)
  ; mutable curr_depth : int
  (* current number of opened boxes *)
  ; mutable queue : atom Queue.t }
and atom =
  { mutable size : int
  ; token : token
  ; length : int }
and token =
  | Text of string
  | Break of int * int
  | Begin of int * box
  | End
  | New_line
  | If_new_line
and box = H | V | HaV | HoV | Box | Fits
and format_atom = Format of box * int
and scan_atom = Scan of int * atom

let enqueue t ({ length; _ } as token) =
  t.right_total <- t.right_total + length ;
  Queue.push token t.queue

let clear_queue t =
  t.left_total <- 1 ;
  t.right_total <- 1 ;
  Queue.clear t.queue

let output_string s k ({ encoder; _} as t) =
  Encoder.write_string s (fun encoder -> k { t with encoder }) encoder

let output_newline k ({ encoder; newline; _ } as t) =
  Encoder.write_string newline (fun encoder -> k { t with encoder }) encoder

let output_spaces n k ({ encoder; _ } as t) =
  Encoder.write_string (String.make n ' ') (fun encoder -> k { t with encoder }) encoder

let break_new_line offset width k =
  output_newline
  @@ fun t ->
  t.is_new_line <- true ;
  let indent = t.margin - width + offset in
  let indent = min t.max_indent indent in
  t.current_indent <- indent ;
  t.space_left <- t.margin - indent ;
  output_spaces t.current_indent k t

let break_line width k t = break_new_line 0 width k t

let break_same_line width k t =
  t.space_left <- t.space_left - width ;
  output_spaces width k t

let force_break_line k t =
  match t.format_stack with
  | [] -> output_newline k t
  | Format (box, width) :: _ ->
    if width > t.space_left
    then match box with
      | Fits | H -> k t
      | V | HaV | HoV | Box ->
        break_line width k t
    else k t

let skip_token k t =
  match Queue.take t.queue with
  | { size; length; _ } ->
    t.left_total <- t.left_total - length ;
    t.space_left <- t.space_left - size ;
    k t
  | exception Queue.Empty -> k t

let compute_token size token k t =
  match token with
  | Text s ->
    t.space_left <- t.space_left - size ;
    output_string s (fun t -> t.is_new_line <- false ; k t) t
  | Begin (off, box) ->
    let insertion_point = t.margin - t.space_left in
    let k t =
      let offset = t.space_left - off in
      let box = match box with
        | V -> V
        | H | HaV | HoV | Box | Fits ->
          if size > t.space_left then box else Fits in
      t.format_stack <- Format (box, offset) :: t.format_stack ;
      k t in
    (if insertion_point > t.max_indent
     then force_break_line k t
     else k t)
  | End ->
    (match t.format_stack with
     | _ :: format_stack -> t.format_stack <- format_stack ; k t
     | [] -> k t)
  | New_line ->
    (match t.format_stack with
     | Format (_, width) :: _ -> break_line width k t
     | [] -> output_newline k t)
  | If_new_line ->
    if t.current_indent <> (t.margin - t.space_left)
    then skip_token k t
    else k t
  | Break (n, off) ->
    (match t.format_stack with
     | [] -> k t
     | Format (box, width) :: _ ->
       match box with
       | HoV ->
         if size > t.space_left
         then break_new_line off width k t
         else break_same_line n k t
       | Box ->
         (* have the line just been broken? *)
         if t.is_new_line
         then break_same_line n k t
         else if size > t.space_left
         then break_new_line off width k t
         (* break the line here leads to new indentation? *)
         else if t.current_indent > t.margin - width + off
         then break_new_line off width k t
         else break_same_line n k t
       | HaV -> break_new_line off width k t
       | Fits -> break_same_line n k t
       | V -> break_new_line off width k t
       | H -> break_same_line n k t)

let infinity = 1000000010

let rec advance k t =
  match Queue.peek t.queue with
  | { size; token; length; } ->
    if not (size < 0 && t.right_total - t.left_total < t.space_left)
    then
      let () = ignore (Queue.take t.queue) in
      let k t =
        t.left_total <- length + t.left_total ;
        advance k t in
      compute_token (if size < 0 then infinity else size) token k t
    else k t
  | exception Queue.Empty -> k t

let advance_left k t =
  try advance k t
  with Queue.Empty -> k t

let enqueue_and_advance token k t =
  enqueue t token ;
  advance_left k t

let make_element size token length =
  { size; token; length; }

let enqueue_string_as size s k t =
  enqueue_and_advance (make_element size (Text s) size) k t

let enqueue_string s k t =
  let size = String.length s in
  enqueue_string_as size s k t

let bottom =
  let first = make_element (-1) (Text "") 0 in
  [ Scan (-1, first) ]

let clear_scan k t =
  t.scan_stack <- bottom ; k t

let set_size break_or_box k t =
  match t.scan_stack with
  | [] -> k t
  | Scan (left_total, ({ size; token; _} as atom)) :: scan_stack ->
    if left_total < t.left_total
    then clear_scan k t
    else match token with
      | Break _ ->
        if break_or_box
        then begin
          atom.size <- t.right_total + size ;
          t.scan_stack <- scan_stack ;
          k t
        end else k t
      | Begin _ ->
        if not break_or_box
        then begin
          atom.size <- t.right_total + size ;
          t.scan_stack <- scan_stack ;
          k t
        end else k t
      | Text _ | End | New_line | If_new_line -> k t

let push scan token k t =
  enqueue t token ;
  let k t =
    t.scan_stack <- Scan (t.right_total, token) :: t.scan_stack ;
    k t in
  if scan then set_size true k t else k t

let open_box_generate indent box k t =
  t.curr_depth <- t.curr_depth + 1 ;
  let atom = make_element (- t.right_total) (Begin (indent, box)) 0 in
  push false atom k t

let open_sys_box k t = open_box_generate 0 HoV k t

let close_box k t =
  if t.curr_depth > 1
  then begin
    enqueue t ({ size= 0; token= End; length= 0 }) ;
    let k t =
      t.curr_depth <- t.curr_depth - 1 ;
      k t in
    set_size true (set_size false k) t
  end else k t

let init k t =
  clear_queue t ;
  let k t =
    t.format_stack <- [] ;
    t.current_indent <- 0 ;
    t.space_left <- t.margin ;
    open_sys_box k t in
  clear_scan k t

let flush new_line k t =
  let rec close_all_box k t =
    if t.curr_depth > 1
    then close_box (close_all_box k) t
    else k t in

  let k t =
    t.right_total <- infinity ;
    advance_left (if new_line then output_newline (init k) else init k) t in

  close_all_box k t

let min_space n k t =
  if n >= 1
  then
    let n = if n < infinity then n else pred infinity in
    t.min_space_left <- n ;
    t.max_indent <- t.margin - t.min_space_left ;
    init k t
  else k t

let as_size size s k t =
  enqueue_string_as size s k t

let string s k t =
  as_size (String.length s) k t

let hbox k t = open_box_generate 0 H k t
let vbox indent k t = open_box_generate indent V k t
let hvbox indent k t = open_box_generate indent HaV k t
let hovbox indent k t = open_box_generate indent HoV k t
let box indent k t = open_box_generate indent Box k t

let new_line k t = flush true k t
let flush k t = flush false k t

let force_newline k t = enqueue_and_advance (make_element 0 New_line 0) k t
let if_new_line k t = enqueue_and_advance (make_element 0 If_new_line 0) k t

let break width offset k t =
  let atom = make_element (- t.right_total) (Break (width, offset)) width in
  push true atom k t

let space k t = break 1 0 k t
let cut k t = break 0 0 k t
let char chr k t = string (String.make 1 chr) k t
