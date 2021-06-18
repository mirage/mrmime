open Angstrom

(* NOTE: contents parser needs to commit ONLY at the end. They are surrounded by
   a best-effort parser (RFC 5322 parser) if they fail. If we commit while we
   compute, [fail] will just fail and Angstrom will not try best-effort parser
   then. [commit] only at the end. *)

let rec index v max idx chr =
  if idx >= max then raise Not_found;
  if Bigstringaf.get v idx = chr then idx else index v max (succ idx) chr

let index v chr = index v (Bigstringaf.length v) 0 chr

let rec end_of_stream ~write_data ~write_line closed dec =
  match Pecu.decode dec with
  | `Await when not closed ->
      Pecu.src dec Bytes.empty 0 0;
      end_of_stream ~write_data ~write_line true dec
  | `Await -> assert false
  | `Data data ->
      write_data data;
      end_of_stream ~write_data ~write_line closed dec
  | `Line line ->
      write_line line;
      end_of_stream ~write_data ~write_line closed dec
  | `End -> Ok ()
  | `Malformed err -> Error err

let rec decode ~write_data ~write_line dec =
  match Pecu.decode dec with
  | `End -> `End
  | `Data data ->
      write_data data;
      decode ~write_data ~write_line dec
  | `Line line ->
      write_line line;
      decode ~write_data ~write_line dec
  | `Await -> `Await
  | `Malformed err -> `Malformed err

let check_end_of_body end_of_body =
  let len = String.length end_of_body in
  Unsafe.peek len Bigstringaf.substring >>| fun str ->
  String.equal end_of_body str

let rec choose ~write_data ~write_line end_of_body dec chunk = function
  | true -> (
      (* at this stage, we are at the end of body. We came from [`Await] case,
         so it's safe to notice to [pecu] the last [chunk]. [trailer] will
         unroll all outputs availables on [pecu]. *)
      let chunk = Bytes.sub chunk 0 (Bytes.length chunk - 1) in
      Pecu.src dec chunk 0 (Bytes.length chunk);
      commit >>= fun () ->
      match end_of_stream ~write_data ~write_line false dec with
      | Ok () -> return ()
      | Error err -> fail err)
  | false ->
      (* at this stage, byte after [chunk] is NOT a part of [end_of_body]. We
         can notice to [pecu] [chunk + end_of_body.[0]], advance on the
         Angstrom's input to one byte, and recall fixpoint until [`Await] case
         (see below). *)
      Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0];
      Pecu.src dec chunk 0 (Bytes.length chunk);
      advance 1 *> commit *> parser ~write_data ~write_line end_of_body dec

and parser ~write_data ~write_line end_of_body dec =
  match decode ~write_data ~write_line dec with
  | `End -> commit
  | `Malformed err -> commit *> fail err
  | `Await -> (
      available >>= function
      | 0 -> peek_char *> parser ~write_data ~write_line end_of_body dec
      | len -> (
          Unsafe.peek len Bigstringaf.sub >>= fun chunk ->
          match index chunk end_of_body.[0] with
          | pos ->
              let tmp = Bytes.create (pos + 1) in
              Bigstringaf.blit_to_bytes chunk ~src_off:0 tmp ~dst_off:0
                ~len:(pos + 1);
              advance pos *> check_end_of_body end_of_body
              <* commit
              >>= choose ~write_data ~write_line end_of_body dec tmp
          | exception Not_found ->
              let chunk =
                Bytes.unsafe_of_string (Bigstringaf.to_string chunk)
              in
              Pecu.src dec chunk 0 (Bytes.length chunk);
              advance len
              *> commit
              *> parser ~write_data ~write_line end_of_body dec))

let parser ~write_data ~write_line end_of_body =
  let dec = Pecu.decoder `Manual in
  parser ~write_data ~write_line end_of_body dec

let with_buffer ?(end_of_line = "\n") end_of_body =
  let buf = Buffer.create 0x100 in
  let write_data x = Buffer.add_string buf x in
  let write_line x =
    Buffer.add_string buf x;
    Buffer.add_string buf end_of_line
  in

  parser ~write_data ~write_line end_of_body >>| fun () -> Buffer.contents buf

let with_emitter ?(end_of_line = "\n") ~emitter end_of_body =
  let write_data x = emitter (Some x) in
  let write_line x = emitter (Some (x ^ end_of_line)) in
  parser ~write_data ~write_line end_of_body

let rec parser ~write_data ~write_line dec =
  match Pecu.decode dec with
  | `End -> commit
  | `Data data ->
      write_data data;
      commit *> parser ~write_data ~write_line dec
  | `Line line ->
      write_line line;
      commit *> parser ~write_data ~write_line dec
  | `Malformed err -> commit *> fail err
  | `Await -> (
      peek_char >>= function
      | None ->
          Pecu.src dec Bytes.empty 0 0;
          commit
      | Some _ ->
          available >>= fun len ->
          Unsafe.take len Bigstringaf.substring >>= fun str ->
          Pecu.src dec (Bytes.unsafe_of_string str) 0 len;
          commit *> parser ~write_data ~write_line dec)

let to_end_of_input ~write_data ~write_line =
  let dec = Pecu.decoder `Manual in
  peek_char *> available >>= take >>= fun str ->
  Pecu.src dec (Bytes.unsafe_of_string str) 0 (String.length str);
  parser ~write_data ~write_line dec
