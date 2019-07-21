include Fancy

let io_buffer_size = 65536
(* XXX(dinosaure): if [margin] is set to [io_buffer_size], we should never add
   an [FWS] token. *)

let create = Pretty.create
let is_empty = Pretty.is_empty
let flush = Pretty.flush
let kflush = Pretty.kflush

let to_string ?(margin= 78) ?(new_line= "\r\n") gen value =
  let buf = Buffer.create 0x100 in

  let emitter =
    let write a x =
      let open Enclosure.IOVec in
      let open Enclosure.Buffer in
      match x with
      | { buffer= String x; off; len; } ->
        Buffer.add_substring buf x off len ; a + len
      | { buffer= Bytes x; off; len; } ->
        Buffer.add_subbytes buf x off len ; a + len
      | { buffer= Bigstring x; off; len; } ->
        let x = Bigstringaf.substring x ~off ~len in
        Buffer.add_string buf x ; a + len in
    List.fold_left write 0 in
  let encoder = Pretty.create
      ~emitter
      ~margin
      ~new_line 0x100 in
  let kend encoder =
    if Pretty.is_empty encoder
    then ()
    else Fmt.failwith "Leave a non-empty encoder" in
  let encoder = eval encoder Fancy.[ !!gen; ] value in
  let () = Pretty.kflush kend encoder in
  Buffer.contents buf

let to_stream ?(margin= 78) ?(new_line= "\r\n") gen value =
  let queue = Queue.create () in
  let line = Buffer.create 4096 in
  let emitter iovecs =
    let write a x =
      let open Enclosure.IOVec in
      let open Enclosure.Buffer in
      match x with
      | { buffer= String x; off; len; } ->
        Buffer.add_substring line x off len ; a + len
      | { buffer= Bytes x; off; len; } ->
        Buffer.add_subbytes line x off len ; a + len
      | { buffer= Bigstring x; off; len; } ->
        let x = Bigstringaf.substring x ~off ~len in
        Buffer.add_string line x ; a + len in
    let len = List.fold_left write 0 iovecs in
    let res = Buffer.contents line in
    if String.length res > 0 then Queue.add res queue ; Buffer.clear line ; len in
  let consumer () = match Queue.pop queue with
    | x -> Some x
    | exception Queue.Empty -> None in
  let encoder = Pretty.create
      ~emitter
      ~margin
      ~new_line 4096 in
  let kend encoder =
    if Pretty.is_empty encoder then ()
    else Fmt.failwith "Leave with a non-empty encoder" in
  let () = keval (Pretty.kflush kend) encoder Fancy.[ !!gen ] value in
  consumer

module IOVec = Enclosure.IOVec
module Buffer = Enclosure.Buffer
