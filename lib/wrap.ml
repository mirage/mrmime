type 'v state = 'v Encoder.state =
  | Flush of
      { continue : int -> 'v state
      ; iovecs : Encoder.IOVec.t list}
  | Continue of
      { continue : Encoder.encoder -> 'v state
      ; encoder: Encoder.encoder}
  | End of 'v

type encoder =
  { encoder : Encoder.encoder
  ; scan_stack : scan_atom Stack.t
  (* the pretty-printer formatting stack. each element is (left_total, element)
     where left_total is the value of left_total when the element has been
     enqueued *)
  ; format_stack : format_atom Stack.t
  (* the pretty-printer formatting stack. each stack element describes a
     pretty-printer box. *)
  ; margin : int
  ; new_line : string
  (* value of the right margin *)
  ; max_indent : int
  (* maximum value of indentation: no box can be opened further *)
  ; mutable space_left : int
  (* space remaining on the current line *)
  ; min_space_left : int
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
  | Value of value
  | Break of int * int
  | Begin of int * box
  | End
  | New_line
  | If_new_line
and box = H | V | HaV | HoV | Box | Fits
and format_atom = Format of box * int
and scan_atom = Scan of int * atom
and value =
  | String of vec * string
  | Bytes of vec * bytes
  | Bigstring of vec * Bigstringaf.t
  | Char of char
  | Uint8 of int
  | LE_uint16 of int
  | BE_uint16 of int
  | LE_uint32 of int32
  | BE_uint32 of int32
  | LE_uint64 of int64
  | BE_uint64 of int64
and vec = { off: int; len: int }

let pp_vec ppf { off; len; } =
  Fmt.pf ppf "{ @[<hov>off = %d;@ len = %d;@] }" off len

let pp_value ppf = function
  | String (vec, _) -> Fmt.pf ppf "(String (%a, #string))" pp_vec vec
  | Bytes (vec, _) -> Fmt.pf ppf "(Bytes (%a, #bytes))" pp_vec vec
  | Bigstring (vec, _) -> Fmt.pf ppf "(Bigstring (%a, #bigstring))" pp_vec vec
  | Char chr -> Fmt.pf ppf "(Char %02x)" (Char.code chr)
  | Uint8 n -> Fmt.pf ppf "(Uint8 %02x)" n
  | LE_uint16 n -> Fmt.pf ppf "(LE_uint16 %04x)" n
  | BE_uint16 n -> Fmt.pf ppf "(BE_uint16 %04x)" n
  | LE_uint32 n -> Fmt.pf ppf "(LE_uint32 %04lx)" n
  | BE_uint32 n -> Fmt.pf ppf "(BE_uint32 %04lx)" n
  | LE_uint64 n -> Fmt.pf ppf "(LE_uint64 %04Lx)" n
  | BE_uint64 n -> Fmt.pf ppf "(BE_uint64 %04Lx)" n

let pp_box ppf = function
  | H -> Fmt.string ppf "<H>"
  | V -> Fmt.string ppf "<V>"
  | HaV -> Fmt.string ppf "<HV>"
  | HoV -> Fmt.string ppf "<HoV>"
  | Box -> Fmt.string ppf "<Box>"
  | Fits -> Fmt.string ppf "<Fits>"

type 'r k0 = (encoder -> 'r state) -> encoder -> 'r state
type ('a, 'r) k1 = 'a -> (encoder -> 'r state) -> encoder -> 'r state

let enqueue t ({ length; _ } as token) =
  t.right_total <- t.right_total + length ;
  Queue.push token t.queue

let clear_queue t =
  t.left_total <- 1 ;
  t.right_total <- 1 ;
  Queue.clear t.queue

let lift
  :
     writer:('x -> (Encoder.encoder -> 'r state) -> Encoder.encoder -> 'r state)
  -> 'x
  -> (encoder -> 'r state)
  -> encoder
  -> 'r state
  = fun ~writer x k t ->
    writer x (fun encoder -> k { t with encoder }) t.encoder

let output_bigstring ~off ~len x k t = lift ~writer:(Encoder.write_bigstring ~off ~len) x k t
let output_bytes ~off ~len x k t = lift ~writer:(Encoder.write_bytes ~off ~len) x k t
let output_string ~off ~len x k t = lift ~writer:(Encoder.write_string ~off ~len) x k t
let output_char x k t = lift ~writer:Encoder.write_char x k t
let output_uint8 x k t = lift ~writer:Encoder.write_uint8 x k t

let output_be_uint16 x k t = lift ~writer:Encoder.BE.write_uint16 x k t
let output_be_uint32 x k t = lift ~writer:Encoder.BE.write_uint32 x k t
let output_be_uint64 x k t = lift ~writer:Encoder.BE.write_uint64 x k t
let output_le_uint16 x k t = lift ~writer:Encoder.LE.write_uint16 x k t
let output_le_uint32 x k t = lift ~writer:Encoder.LE.write_uint32 x k t
let output_le_uint64 x k t = lift ~writer:Encoder.LE.write_uint64 x k t

let continue k t = Encoder.continue (fun encoder -> k { t with encoder }) t.encoder

let output_value v k t =
  Fmt.epr "Output value: %a.\n%!" pp_value v ;

  match v with
  | String ({ off; len; }, x) -> output_string ~off ~len x k t
  | Bytes ({ off; len; }, x) -> output_bytes ~off ~len x k t
  | Bigstring ({ off; len; }, x) -> output_bigstring ~off ~len x k t
  | Char x -> output_char x k t
  | Uint8 x -> output_uint8 x k t
  | LE_uint16 x -> output_le_uint16 x k t
  | LE_uint32 x -> output_le_uint32 x k t
  | LE_uint64 x -> output_le_uint64 x k t
  | BE_uint16 x -> output_be_uint16 x k t
  | BE_uint32 x -> output_be_uint32 x k t
  | BE_uint64 x -> output_be_uint64 x k t

let output_new_line k ({ encoder; new_line; _ } as t) =
  Fmt.epr "Output new line.\n%!" ;
  Encoder.write_string new_line (fun encoder -> k { t with encoder }) encoder

let output_spaces n k ({ encoder; _ } as t) =
  Fmt.epr "Output space (%d space(s)).\n%!" n ;
  if n >= 0
  then Encoder.write_string (String.make n ' ') (fun encoder -> k { t with encoder }) encoder
  else k t

let break_new_line offset width k =
  Fmt.epr "Break new line.\n%!" ;
  output_new_line
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
  match Stack.top t.format_stack with
  | Format (box, width) ->
    if width > t.space_left
    then match box with
      | Fits | H -> k t
      | V | HaV | HoV | Box ->
        break_line width k t
    else k t
  | exception Stack.Empty ->
    Fmt.epr "Force break line.\n%!" ;
    output_new_line k t

let skip_token k t =
  match Queue.take t.queue with
  | { size; length; _ } ->
    t.left_total <- t.left_total - length ;
    t.space_left <- t.space_left - size ;
    k t
  | exception Queue.Empty -> k t

let compute_token size token k t =
  match token with
  | Value v ->
    t.space_left <- t.space_left - size ;
    output_value v (fun t -> t.is_new_line <- false ; k t) t
  | Begin (indent, box) ->
    let insertion_point = t.margin - t.space_left in
    Fmt.epr "Begin (indent:%d, %a), insertion_point: %d.\n%!"
      indent pp_box box insertion_point ;
    Fmt.epr "insertion_point:%d > max_indent:%d.\n%!" insertion_point t.max_indent ;
    let k t =
      let width = t.space_left - indent in
      let box = match box with
        | V -> V
        | H | HaV | HoV | Box | Fits ->
          if size > t.space_left then box else Fits in
      Stack.push (Format (box, width)) t.format_stack ;
      k t in
    (if insertion_point > t.max_indent
     then force_break_line k t
     else k t)
  | End ->
    (try ignore @@ Stack.pop t.format_stack with Stack.Empty -> ()) ;
    k t
  | New_line ->
    (match Stack.pop t.format_stack with
     | Format (_, width) -> break_line width k t
     | exception Stack.Empty -> output_new_line k t)
  | If_new_line ->
    if t.current_indent <> (t.margin - t.space_left)
    then skip_token k t
    else k t
  | Break (n, off) ->
    Fmt.epr "Break (n:%d, off:%d).\n%!" n off ;
    match Stack.top t.format_stack with
    | Format (box, width) ->
      (match box with
       | HoV ->
         Fmt.epr "HoV box size:%d > t.space_left:%d.\n%!" size t.space_left ;
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
    | exception Stack.Empty -> k t

let infinity = 1000000010

let rec advance k t =
  match Queue.peek t.queue with
  | { size; token; length; } ->
    if size >= 0 || t.right_total - t.left_total >= t.space_left
    then
      let () = ignore (Queue.take t.queue) in
      let k t =
        t.left_total <- length + t.left_total ;
        (advance[@tailcall]) k t in
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

let enqueue_value_as size x k t =
  enqueue_and_advance (make_element size (Value x) size) k t

let clear_scan k t =
  Stack.clear t.scan_stack ;
  let first = make_element (-1) (Value (String ({ off= 0; len= 0; }, ""))) 0 in
  Stack.push (Scan (-1, first)) t.scan_stack ;
  k t

let set_size break_or_box k t =
  match Stack.top t.scan_stack with
  | Scan (left_total, ({ size; token; _} as atom)) ->
    (if left_total < t.left_total
     then clear_scan k t
     else match token with
       | Break _ ->
         if break_or_box
         then begin
           atom.size <- t.right_total + size ;
           ignore @@ Stack.pop t.scan_stack ;
           k t
         end else k t
       | Begin _ ->
         if not break_or_box
         then begin
           atom.size <- t.right_total + size ;
           ignore @@ Stack.pop t.scan_stack ;
           k t
         end else k t
       | Value _ | End | New_line | If_new_line -> k t)
  | exception Stack.Empty -> k t

let push scan token k t =
  enqueue t token ;
  let k t =
    Stack.push (Scan (t.right_total, token)) t.scan_stack ;
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
    Fmt.epr "Init.\n%!" ;

    Stack.clear t.format_stack ;
    t.current_indent <- 0 ;
    t.curr_depth <- 0 ;
    t.space_left <- t.margin ;
    open_sys_box k t in
  clear_scan k t

let schedule_flush f t =
  { t with encoder = Encoder.schedule_flush (fun n encoder -> f n { t with encoder }) t.encoder }

let flush new_line k t =
  let rec close_all_box k t =
    if t.curr_depth > 1
    then close_box (close_all_box k) t
    else k t in

  let k t =
    t.right_total <- infinity ;
    let k t = Encoder.flush (fun encoder -> k { t with encoder }) t.encoder in
    advance_left (if new_line then output_new_line (init k) else init k) t in

  close_all_box k t

let as_size size s k t = enqueue_value_as size s k t

let write_bigstring ?(off = 0) ?len x k t =
  let size = match len with
    | Some len -> len
    | None -> Bigstringaf.length x - off in
  let vec = { off; len= size } in
  as_size size (Bigstring (vec, x)) k t

let write_string ?(off = 0) ?len x k t =
  let size = match len with
    | Some len -> len
    | None -> String.length x - off in
  let vec = { off; len= size; } in
  as_size size (String (vec, x)) k t

let write_bytes ?(off = 0) ?len x k t =
  let size = match len with
    | Some len -> len
    | None -> Bytes.length x - off in
  let vec = { off; len= size } in
  as_size size (Bytes (vec, x)) k t

let write_char x k t = as_size 1 (Char x) k t
let write_uint8 x k t = as_size 1 (Uint8 x) k t

module LE = struct
  let write_uint16 x k t = as_size 2 (LE_uint16 x) k t
  let write_uint32 x k t = as_size 4 (LE_uint32 x) k t
  let write_uint64 x k t = as_size 8 (LE_uint64 x) k t
end

module BE = struct
  let write_uint16 x k t = as_size 2 (BE_uint16 x) k t
  let write_uint32 x k t = as_size 4 (BE_uint32 x) k t
  let write_uint64 x k t = as_size 8 (BE_uint64 x) k t
end

let hbox k t = open_box_generate 0 H k t
let vbox indent k t = open_box_generate indent V k t
let hvbox indent k t = open_box_generate indent HaV k t
let hovbox indent k t = open_box_generate indent HoV k t
let box indent k t = open_box_generate indent Box k t

let new_line k t = flush true k t
let flush k t = flush false k t

let force_new_line k t = enqueue_and_advance (make_element 0 New_line 0) k t
let if_new_line k t = enqueue_and_advance (make_element 0 If_new_line 0) k t

let break width offset k t =
  let atom = make_element (- t.right_total) (Break (width, offset)) width in
  push true atom k t

let space k t = break 1 0 k t
let cut k t = break 0 0 k t

let create ?(margin = 998) ?(new_line = "\r\n") len =
  let encoder = Encoder.create len in
  let sys_token = make_element (-1) (Begin (0, HoV)) 0 in
  let queue = Queue.create () in
  Queue.add sys_token queue ;
  let scan_stack = Stack.create () in
  Stack.push (Scan (1, sys_token)) scan_stack ;
  let min_space_left = 10 in
  let max_indent =
    let max_indent = 78 - min_space_left in
    if max_indent <= margin
    then max_indent
    else max (max (margin - min_space_left) (margin / 2)) 1 in
  { encoder
  ; scan_stack
  ; format_stack = Stack.create ()
  ; margin
  ; max_indent
  ; space_left = margin
  ; min_space_left
  ; current_indent = 0
  ; left_total = 1
  ; right_total = 1
  ; curr_depth = 1
  ; is_new_line = true
  ; new_line
  ; queue }
