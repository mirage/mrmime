module Level0 = Level0
module Level1 = Level1

module MakeFormat = Format.Make
module MakeBox = Box.Make

include Format.Make(Level1)

type 'a state = 'a Level0.state

let io_buffer_size = 65536
(* XXX(dinosaure): if [margin] is set to [io_buffer_size], we should never add
   an [FWS] token. *)

let to_string ?(new_line= "\r\n") gen value =
  let buf = Buffer.create 0x100 in

  let writer_of_buffer =
    let write a x =
      let open Level0.IOVec in
      let open Level0.Buffer in
      match x with
      | { buffer= String x; off; len; } ->
        Buffer.add_substring buf x off len ; a + len
      | { buffer= Bytes x; off; len; } ->
        Buffer.add_subbytes buf x off len ; a + len
      | { buffer= Bigstring x; off; len; } ->
        let x = Bigstringaf.substring x ~off ~len in
        Buffer.add_string buf x ; a + len in
    List.fold_left write 0 in
  let encoder = Level1.create
      ~margin:io_buffer_size
      ~new_line 0x100 in
  let encoder = with_writer encoder writer_of_buffer in
  let _ = eval encoder [ !!gen; yield ] value in
  (* TODO: verify if [encoder] is empty. *)
  Buffer.contents buf
