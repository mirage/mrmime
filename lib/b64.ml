open Angstrom

(* NOTE: contents parser needs to commit ONLY at the end. They are surrounded by
   a best-effort parser (RFC 5322 parser) if they fail. If we commit while we
   compute, [fail] will just fail and Angstrom will not try best-effort parser
   then. [commit] only at the end. *)

let rec index v max idx chr =
  if idx >= max then raise Not_found;
  if Bigstringaf.get v idx = chr then idx else index v max (succ idx) chr

let index v chr = index v (Bigstringaf.length v) 0 chr

let rec end_of_stream ~write_data closed dec =
  match Base64_rfc2045.decode dec with
  | `Await when not closed ->
      Base64_rfc2045.src dec Bytes.empty 0 0;
      end_of_stream ~write_data true dec
  | `Await -> assert false
  | `Flush data ->
      write_data data;
      end_of_stream ~write_data closed dec
  | `Malformed err -> Error err
  | `Wrong_padding -> Error "wrong padding"
  | `End -> Ok ()

let rec decode ~write_data dec =
  match Base64_rfc2045.decode dec with
  | `Await -> `Await
  | `Flush data ->
      write_data data;
      decode ~write_data dec
  | `Malformed err -> `Malformed err
  | `Wrong_padding -> `Wrong_padding
  | `End -> `End

let check_end_of_body end_of_body =
  let len = String.length end_of_body in
  Unsafe.peek len Bigstringaf.substring >>| String.equal end_of_body

let rec choose ~write_data end_of_body dec chunk = function
  | true -> (
      let chunk = Bytes.sub chunk 0 (Bytes.length chunk - 1) in
      Base64_rfc2045.src dec chunk 0 (Bytes.length chunk);
      commit >>= fun () ->
      match end_of_stream ~write_data false dec with
      | Ok () -> return ()
      | Error err -> fail err)
  | false ->
      Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0];
      Base64_rfc2045.src dec chunk 0 (Bytes.length chunk);
      advance 1 *> commit *> parser ~write_data end_of_body dec

and parser ~write_data end_of_body dec =
  match decode ~write_data dec with
  | `End -> commit
  | `Malformed err -> fail err
  | `Wrong_padding -> fail "wrong padding"
  | `Await -> (
      available >>= function
      | 0 -> peek_char *> parser ~write_data end_of_body dec
      | len -> (
          Unsafe.peek len Bigstringaf.sub >>= fun chunk ->
          match index chunk end_of_body.[0] with
          | pos ->
              let tmp = Bytes.create (pos + 1) in
              Bigstringaf.blit_to_bytes chunk ~src_off:0 tmp ~dst_off:0
                ~len:(pos + 1);
              advance pos *> check_end_of_body end_of_body
              <* commit
              >>= choose ~write_data end_of_body dec tmp
          | exception Not_found ->
              let chunk =
                Bytes.unsafe_of_string (Bigstringaf.to_string chunk)
              in
              Base64_rfc2045.src dec chunk 0 (Bytes.length chunk);
              advance len *> commit *> parser ~write_data end_of_body dec))

let parser ~write_data end_of_body =
  let dec = Base64_rfc2045.decoder `Manual in
  parser ~write_data end_of_body dec

let with_buffer end_of_body =
  let buf = Buffer.create 0x100 in
  let write_data x = Buffer.add_string buf x in

  parser ~write_data end_of_body >>| fun () -> Buffer.contents buf

let with_emitter ~emitter end_of_body =
  let write_data x = emitter (Some x) in
  parser ~write_data end_of_body

let rec parser ~write_data dec =
  match Base64_rfc2045.decode dec with
  | `End -> commit
  | `Flush data ->
      write_data data;
      commit *> parser ~write_data dec
  | `Malformed err -> fail err
  | `Wrong_padding -> fail "wrong padding"
  | `Await -> (
      peek_char >>= function
      | None ->
          Base64_rfc2045.src dec Bytes.empty 0 0;
          commit *> parser ~write_data dec
      | Some _ ->
          available >>= fun len ->
          Unsafe.take len Bigstringaf.substring >>= fun str ->
          Base64_rfc2045.src dec (Bytes.unsafe_of_string str) 0 len;
          commit *> parser ~write_data dec)

let to_end_of_input ~write_data =
  let dec = Base64_rfc2045.decoder `Manual in
  peek_char *> available >>= take >>= fun str ->
  Base64_rfc2045.src dec (Bytes.unsafe_of_string str) 0 (String.length str);
  parser ~write_data dec
