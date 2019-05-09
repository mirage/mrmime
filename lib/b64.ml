open Angstrom

(* NOTE: contents parser needs to commit ONLY at the end. They are surrounded by
   a best-effort parser (RFC 5322 parser) if they fail. If we commit while we
   compute, [fail] will just fail and Angstrom will not try best-effort parser
   then. [commit] only at the end. *)

let parser ~write_data end_of_body =
  let dec = Base64_rfc2045.decoder `Manual in

  let check_end_of_body =
    let expected_len = String.length end_of_body in
    Unsafe.peek expected_len
      (fun ba ~off ~len ->
         let raw = Bigstringaf.substring ba ~off ~len in
         String.equal raw end_of_body) in

  let trailer () =
    let rec finish () = match Base64_rfc2045.decode dec with
      | `Await -> assert false
      | `Flush data -> write_data data ; finish ()
      | `Malformed err -> fail err
      | `Wrong_padding -> fail "wrong padding"
      | `End -> commit

    and go () = match Base64_rfc2045.decode dec with
      | `Await ->
        Base64_rfc2045.src dec Bytes.empty 0 0 ; finish ()
      | `Flush data -> write_data data ; go ()
      | `Malformed err -> fail err
      | `Wrong_padding -> fail "wrong padding"
      | `End -> commit in

    go () in

  fix @@ fun m ->
  let choose chunk = function
    | true ->
      let chunk = Bytes.sub chunk 0 (Bytes.length chunk - 1) in
      Base64_rfc2045.src dec chunk 0 (Bytes.length chunk) ; trailer ()
    | false ->
      Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0] ;
      Base64_rfc2045.src dec chunk 0 (Bytes.length chunk) ;
      advance 1 *> m in

  Unsafe.take_while ((<>) end_of_body.[0]) Bigstringaf.substring
  >>= fun chunk ->
  let rec go () = match Base64_rfc2045.decode dec with
    | `End -> commit
    | `Await ->
      let chunk' = Bytes.create (String.length chunk + 1) in
      Bytes.blit_string chunk 0 chunk' 0 (String.length chunk) ;
      check_end_of_body >>= choose chunk'
    | `Flush data ->
      write_data data ; go ()
    | `Malformed err -> fail err
    | `Wrong_padding -> fail "wrong padding" in
  go ()

let with_buffer end_of_body =
  let buf = Buffer.create 0x100 in
  let write_data x = Buffer.add_string buf x in

  parser ~write_data end_of_body >>| fun () -> Buffer.contents buf

let with_emitter ~emitter end_of_body =
  let write_data x = emitter (Some x) in
  parser ~write_data end_of_body

let to_end_of_input ~write_data =
  let dec = Base64_rfc2045.decoder `Manual in

  fix @@ fun m -> match Base64_rfc2045.decode dec with
  | `End -> commit
  | `Await ->
    (peek_char >>= function
      | None -> Base64_rfc2045.src dec Bytes.empty 0 0 ; return ()
      | Some _ -> available >>= fun n -> Unsafe.take n
          (fun ba ~off ~len ->
             let chunk = Bytes.create len in
             Bigstringaf.blit_to_bytes ba ~src_off:off chunk ~dst_off:0 ~len ;
             Base64_rfc2045.src dec chunk 0 len)
        >>= fun () -> m)
  | `Flush data -> write_data data ; m
  | `Malformed err -> fail err
  | `Wrong_padding -> fail "wrong padding"
