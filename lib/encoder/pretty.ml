type vec = { off: int; len: int; }
type box = Box | TBox of int | BBox

type value =
  | String of vec * string
  | Bytes of vec * bytes
  | Bigstring of vec * Bigstringaf.t

let split_value len x =
  assert (len > 0) ;

  match x with
  | String (vec, v) ->
    let v0 = String.sub v 0 len in
    let v1 = String.sub v len (vec.len - len) in
    String ({ off= 0; len; }, v0), String ({ off= 0; len= vec.len - len }, v1)
  | Bytes (vec, v) ->
    let v0 = Bytes.sub v 0 len in
    let v1 = Bytes.sub v len (vec.len - len) in
    Bytes ({ off= 0; len; }, v0), Bytes ({ off= 0; len= vec.len - len}, v1)
  | Bigstring (vec, v) ->
    Bigstring ({ off= vec.off; len}, v), Bigstring ({ off= vec.off + len; len= vec.len - len }, v)

let length_of_value = function
  | String (vec, _) | Bytes (vec, _) | Bigstring (vec, _) -> vec.len

type atom =
  | Breakable of value
  | Unbreakable of value
  | Break of { len : int; indent : int; }
  | New_line
  | Open of box
  | Close

let box = Box
let tbox indent = TBox indent
let bbox = BBox
let o box = Open box
let new_line = New_line
let close = Close
let fws = Break { len= 1; indent= 1; }
let spaces len = Break { len; indent= 0; }
let break ~len ~indent = Break { len; indent; }

let v ~breakable = match breakable with
  | true -> fun x -> Breakable x
  | false -> fun x -> Unbreakable x

let string ?(breakable= false) ?(off= 0) ?len x =
  let len = match len with
    | Some len -> len | None -> String.length x - off in
  let value = String ({ off; len; }, x) in
  v ~breakable value

let bytes ?(breakable= false) ?(off= 0) ?len x =
  let len = match len with
    | Some len -> len | None -> Bytes.length x - off in
  let value = Bytes ({ off; len; }, x) in
  v ~breakable value

let bigstring ?(breakable= false) ?(off= 0) ?len x =
  let len = match len with
    | Some len -> len | None -> Bigstringaf.length x - off in
  let value = Bigstring ({ off; len; }, x) in
  v ~breakable value

type token =
  | TValue of value
  | TBreak of int
  | TBox of [ `Root | `Box | `Indent of int ]
  | TClose

let length_of_token = function
  | TValue value -> length_of_value value
  | TBreak len -> len
  | TBox _ -> 0
  | TClose -> 0

module Option = struct
  let bind x f = match x with
    | Some x -> f x
    | None -> None

  let ( >>= ) = bind
end

module Stack : sig
  type +'a t

  val empty : _ t
  val push : 'a -> 'a t -> 'a t
  val swap_exn : ('a -> 'a) -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val tail_exn : 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val pp : 'a Fmt.t -> 'a t Fmt.t
end = struct
  type 'a t = 'a list

  let empty = []
  let push x t = x :: t

  exception Empty

  let swap_exn f = function
    | [] -> raise Empty
    | x :: r -> f x :: r

  let pop = function
    | [] -> None
    | x :: r -> Some (x, r)

  let fold = List.fold_left

  let tail_exn = function
    | _ :: r -> r
    | [] -> raise Empty

  let map f l = List.map f l
  let pp = Fmt.Dump.list
end

module Queue = Ke.Fke

type t =
  { boxes : [ `Root | `Box | `Indent of int ] Stack.t
  ; breaks : [ `Indent of int ] list Stack.t
  ; inner : int list Stack.t
  ; indent : int
  ; margin : int
  ; new_line : string
  ; queue : token Queue.t
  ; encoder : Enclosure.encoder }

let pp_box ppf = function
  | `Root -> Fmt.string ppf "`Root"
  | `Box -> Fmt.string ppf "`Box"
  | `Indent n -> Fmt.pf ppf "(`Indent %d)" n

let pp_break ppf (`Indent n) = Fmt.pf ppf "(`Indent %d)" n

let pp_token ppf = function
  | TValue (String ({ off; len; }, x)) -> Fmt.pf ppf "%S" (String.sub x off len)
  | TValue (Bytes ({ off; len; }, x)) -> Fmt.pf ppf "%S" (Bytes.sub_string x off len)
  | TValue (Bigstring ({ off; len; }, x)) -> Fmt.pf ppf "%S" (Bigstringaf.substring x ~off ~len)
  | TBreak len -> Fmt.pf ppf "%S" (String.make len ' ')
  | TBox `Box -> Fmt.pf ppf "["
  | TBox (`Indent n) -> Fmt.pf ppf "[<%d>" n
  | TBox `Root -> Fmt.pf ppf "[<root>"
  | TClose -> Fmt.pf ppf "]"

let pp ppf t =
  Fmt.pf ppf "{ @[<hov>boxes= @[<hov>%a@];@ \
                       breaks= @[<hov>%a@];@ \
                       inner= @[<hov>%a@];@ \
                       indent= %d;@ \
                       margin= %d;@ \
                       new_line= %S;@ \
                       queue= @[<hov>%a@];@ \
                       encoder= @[<hov>%a@];@] }"
    (Stack.pp pp_box) t.boxes
    (Stack.pp (Fmt.Dump.list pp_break)) t.breaks
    (Stack.pp Fmt.(Dump.list int)) t.inner
    t.indent t.margin t.new_line
    (Queue.pp pp_token) t.queue
    Enclosure.pp t.encoder

let ( <.> ) f g = fun x -> f (g x)
let flip f = fun a b -> f b a

let merge_breaks token (queue, x) = match token, x with
  | Break { len= len_x; _ }, TBreak len  ->
    Some (queue, len_x + len)
  | _, _ -> None

let current_length_of_line t =
  t.indent + Queue.fold (flip ((+) <.> length_of_token)) 0 t.queue

let emit_line k t =
  let rec go queue encoder = match Queue.pop queue with
    | Some (TValue (String ({ off; len; }, v)), queue) ->
      Enclosure.kschedule_string (go queue) encoder ~off ~len v
    | Some (TValue (Bytes ({ off; len; }, v)), queue) ->
      Enclosure.kschedule_bytes (go queue) encoder ~off ~len v
    | Some (TValue (Bigstring ({ off; len; }, v)), queue) ->
      Enclosure.kschedule_bigstring (go queue) encoder ~off ~len v
    | Some (TBreak len, queue) ->
      Enclosure.kschedule_string (go queue) encoder ~len (String.make len ' ')
    | Some (TBox _, queue) | Some (TClose, queue) -> go queue encoder
    | None ->
      let k encoder = k { t with encoder; queue= Queue.empty } in
      let k encoder =
        Enclosure.flush k encoder in
      Enclosure.kschedule_string k encoder t.new_line in
  Enclosure.kschedule_string (go t.queue) t.encoder (String.make t.indent ' ')

let merge_indents k t =
  let indent_by_box = Stack.fold (fun a -> function `Box | `Root -> a | `Indent n -> a + n) 0 t.boxes in
  let indent_by_break =
    let k a l = List.fold_left (fun a -> function `Indent n -> (max : int -> int -> int) a n) a l in
    Stack.fold k 0 t.breaks in
  k { t with indent= indent_by_box + indent_by_break
           ; inner= Stack.map (fun _ -> []) t.boxes
           ; breaks= Stack.map (fun _ -> []) t.boxes }

let rec kpush_breakable_value ~current_length_of_line k value t =
  if current_length_of_line >= t.margin
  then emit_line (merge_indents (kpush k (Breakable value))) t
  else if current_length_of_line + length_of_token (TValue value) > t.margin
  then
    let len = t.margin - current_length_of_line in
    let value0, value1 = split_value len value in
    let token0 = TValue value0 in
    let token1 = v ~breakable:true value1 in
    emit_line (merge_indents (kpush k token1)) { t with queue= Queue.push t.queue token0 }
  else k { t with queue= Queue.push t.queue (TValue value)
                ; inner= Stack.swap_exn (fun lenv -> length_of_value value :: lenv) t.inner }

and kpush_unbreakable_value ~current_length_of_line k value t =
  if current_length_of_line + length_of_token (TValue value) > t.margin && not (Queue.is_empty t.queue)
  then ( match Queue.pop t.queue with
         | Some (TBreak _, queue) ->
           emit_line (merge_indents (kpush k (Unbreakable value))) { t with queue }
         | Some _ | None ->
           emit_line (merge_indents (kpush k (Unbreakable value))) t )
  else k { t with queue= Queue.push t.queue (TValue value)
                ; inner= Stack.swap_exn (fun lenv -> length_of_value value :: lenv) t.inner }

and kpush k value t =
  let current_length_of_line = current_length_of_line t in

  let append stack len =
    match Stack.pop stack with
    | Some (lenv, stack) -> Stack.push (len :: lenv) stack
    | None -> assert false in

  match value with
  | New_line -> emit_line (merge_indents k) t
  | Open Box ->
    k { t with boxes= Stack.push `Box t.boxes
             ; inner= Stack.push [] t.inner
             ; breaks= Stack.push [] t.breaks
             ; queue= Queue.push t.queue (TBox `Box) }
  | Open (TBox indent) ->
    k { t with boxes= Stack.push (`Indent indent) t.boxes
             ; inner= Stack.push [] t.inner
             ; breaks= Stack.push [] t.breaks
             ; queue= Queue.push t.queue (TBox (`Indent indent)) }
  | Open BBox ->
    let indent = Stack.fold (flip ((+) <.> (List.fold_left (+) 0))) 0 t.inner in
    k { t with boxes= Stack.push (`Indent indent) t.boxes
             ; inner= Stack.push [] t.inner
             ; breaks= Stack.push [] t.breaks
             ; queue= Queue.push t.queue (TBox (`Indent indent)) }
  | Close ->
    (* XXX(dinosaure): check [`Root] box. *)
    k { t with boxes= Stack.tail_exn t.boxes
             ; inner= Stack.tail_exn t.inner
             ; breaks= Stack.tail_exn t.breaks
             ; queue= Queue.push t.queue TClose }
  | Breakable value -> kpush_breakable_value ~current_length_of_line k value t
  | Unbreakable value -> kpush_unbreakable_value ~current_length_of_line k value t
  | Break { len; indent; } as break ->
    match let open Option in Queue.tail t.queue >>= merge_breaks break with
    | Some (queue, len) ->
      if current_length_of_line + length_of_token (TBreak len) >= t.margin
      then ( emit_line (merge_indents k) { t with queue
                                                ; breaks= append t.breaks (`Indent indent) } )
      else k { t with queue= Queue.push queue (TBreak len)
                    ; inner= append t.inner len
                    ; breaks= append t.breaks (`Indent indent) }
    | None ->
      if current_length_of_line + length_of_token (TBreak len) > t.margin
      then ( emit_line (merge_indents k) { t with breaks= append t.breaks (`Indent indent) } )
      else k { t with queue= Queue.push t.queue (TBreak len)
                    ; inner= append t.inner len
                    ; breaks= append t.breaks (`Indent indent) }

let kflush k t = Enclosure.flush (fun encoder -> k { t with encoder }) t.encoder

external identity : 'a -> 'a = "%identity"

let push = kpush identity
let flush = kflush identity

let is_empty t =
  Queue.is_empty t.queue && Enclosure.is_empty t.encoder

let create ?(margin= 998) ?(new_line= "\r\n") ~emitter len =
  let encoder = Enclosure.create ~emitter len in
  { encoder
  ; queue= Queue.empty
  ; boxes= Stack.push `Root Stack.empty
  ; breaks= Stack.push [] Stack.empty
  ; inner= Stack.push [] Stack.empty
  ; indent= 0
  ; margin; new_line; }
