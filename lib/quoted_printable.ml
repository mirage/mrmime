open Angstrom

(* NOTE: contents parser needs to commit ONLY at the end. They are surrounded by
   a best-effort parser (RFC 5322 parser) if they fail. If we commit while we
   compute, [fail] will just fail and Angstrom will not try best-effort parser
   then. [commit] only at the end. *)

let parser ~write_data ~write_line end_of_body =
  let dec = Pecu.decoder `Manual in

  let check_end_of_body =
    let expected_len = String.length end_of_body in
    Unsafe.peek expected_len (fun ba ~off ~len ->
        let raw = Bigstringaf.substring ba ~off ~len in
        String.equal raw end_of_body)
  in

  let trailer () =
    let rec finish () =
      match Pecu.decode dec with
      | `Await -> assert false
      (* on [pecu], because [finish] was called just before [Pecu.src dec
         Bytes.empty 0 0] (so, when [len = 0]), semantically, it's impossible to
         retrieve this case. If [pecu] expects more inputs and we noticed end of
         input, it will return [`Malformed]. *)
      | `Data data ->
          write_data data;
          finish ()
      | `Line line ->
          write_line line;
          finish ()
      | `End -> commit
      | `Malformed err -> fail err
    and go () =
      match Pecu.decode dec with
      | `Await ->
          (* definitely [end_of_body]. *)
          Pecu.src dec Bytes.empty 0 0;
          finish ()
      | `Data data ->
          write_data data;
          go ()
      | `Line line ->
          write_line line;
          go ()
      | `End -> commit
      | `Malformed err -> fail err
    in

    go ()
  in

  fix @@ fun m ->
  let choose chunk = function
    | true ->
        (* at this stage, we are at the end of body. We came from [`Await] case,
           so it's safe to notice to [pecu] the last [chunk]. [trailer] will
           unroll all outputs availables on [pecu]. *)
        let chunk = Bytes.sub chunk 0 (Bytes.length chunk - 1) in
        Pecu.src dec chunk 0 (Bytes.length chunk);
        trailer ()
    | false ->
        (* at this stage, byte after [chunk] is NOT a part of [end_of_body]. We
           can notice to [pecu] [chunk + end_of_body.[0]], advance on the
           Angstrom's input to one byte, and recall fixpoint until [`Await] case
           (see below). *)
        Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0];
        Pecu.src dec chunk 0 (Bytes.length chunk);
        advance 1 *> m
  in

  (* take while we did not discover the first byte of [end_of_body]. *)
  Unsafe.take_while (( <> ) end_of_body.[0]) Bigstringaf.substring
  >>= fun chunk ->
  (* start to know what we need to do with [pecu]. *)
  let rec go () =
    match Pecu.decode dec with
    | `End -> commit
    | `Await ->
        (* [pecu] expects inputs. At this stage, we know that after [chunk], we
           have the first byte of [end_of_body] - but we don't know if we have
           [end_of_body] or a part of it.

           [check_end_of_body] will advance to see if we really have
           [end_of_body]. The result will be sended to [choose]. *)
        let chunk' = Bytes.create (String.length chunk + 1) in
        Bytes.blit_string chunk 0 chunk' 0 (String.length chunk);
        check_end_of_body >>= choose chunk'
    | `Data data ->
        write_data data;
        go ()
    | `Line line ->
        write_line line;
        go ()
    | `Malformed err -> fail err
  in
  go ()

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
      parser ~write_data ~write_line dec
  | `Line line ->
      write_line line;
      parser ~write_data ~write_line dec
  | `Malformed err -> fail err
  | `Await -> (
      peek_char >>= function
      | None ->
          let () = Pecu.src dec Bytes.empty 0 0 in
          return ()
      | Some _ ->
          available >>= take <* commit >>= fun str ->
          let () =
            Pecu.src dec (Bytes.unsafe_of_string str) 0 (String.length str)
          in
          parser ~write_data ~write_line dec)

let to_end_of_input ~write_data ~write_line =
  let dec = Pecu.decoder `Manual in
  peek_char *> available >>= take >>= fun str ->
  Pecu.src dec (Bytes.unsafe_of_string str) 0 (String.length str);
  parser ~write_data ~write_line dec
