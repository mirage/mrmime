type 'a elt = { header : Header.t; body : 'a; }

type 'a t =
  | Leaf of 'a elt
  | Multipart of 'a t option list elt
  | Message of 'a t elt

let parser ~write_line end_of_body =
  let open Angstrom in
  let check_end_of_body =
    let expected_len = String.length end_of_body in
    Unsafe.peek expected_len
      (fun ba ~off ~len ->
         let raw = Bigstringaf.substring ba ~off ~len in
         String.equal raw end_of_body) in

  fix @@ fun m ->
  let choose chunk = function
    | true ->
      let chunk = Bytes.sub_string chunk 0 (Bytes.length chunk - 1) in
      write_line chunk ; commit
    | false ->
      Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0] ;
      write_line (Bytes.unsafe_to_string chunk) ;
      advance 1 *> m in

  Unsafe.take_while ((<>) end_of_body.[0]) Bigstringaf.substring
  >>= fun chunk ->
  let chunk' = Bytes.create (String.length chunk + 1) in
  Bytes.blit_string chunk 0 chunk' 0 (String.length chunk) ;
  check_end_of_body >>= choose chunk'

let with_buffer ?(end_of_line = "\n") end_of_body =
  let buf = Buffer.create 0x100 in
  let write_line x =
    Buffer.add_string buf x ;
    Buffer.add_string buf end_of_line in

  let open Angstrom in
  parser ~write_line end_of_body >>| fun () -> Buffer.contents buf

let with_emitter ?(end_of_line = "\n") ~emitter end_of_body =
  let write_line x = emitter (Some (x ^ end_of_line)) in
  parser ~write_line end_of_body

let to_end_of_input ~write_data =
  let open Angstrom in

  fix @@ fun m ->
  peek_char >>= function
  | None -> commit
  | Some _ ->
    available >>= fun n -> Unsafe.take n
      (fun ba ~off ~len ->
         let chunk = Bytes.create len in
         Bigstringaf.blit_to_bytes ba ~src_off:off chunk ~dst_off:0 ~len ;
         write_data (Bytes.unsafe_to_string chunk))
    >>= fun () -> m

let heavy_octet boundary header =
  let open Angstrom in
  match boundary with
  | None ->
    let buf = Buffer.create 0x800 in
    let write_line line =
      Buffer.add_string buf line ;
      Buffer.add_string buf "\n" in
    let write_data = Buffer.add_string buf in
    (match Header.content_encoding header with
     | `Quoted_printable ->
       Quoted_printable.to_end_of_input ~write_data ~write_line
     | `Base64 ->
       B64.to_end_of_input ~write_data
     | `Bit7 | `Bit8 | `Binary ->
       to_end_of_input ~write_data
     | `Ietf_token _x | `X_token _x -> assert false)
    >>| fun () -> Buffer.contents buf
  | Some boundary ->
    let end_of_body = Rfc2046.make_delimiter boundary in
    match Header.content_encoding header with
    | `Quoted_printable ->
      Quoted_printable.with_buffer end_of_body
    | `Base64 ->
      B64.with_buffer end_of_body
    | `Bit7 | `Bit8 | `Binary ->
      with_buffer end_of_body
    | `Ietf_token _x | `X_token _x -> assert false

let light_octet ~emitter boundary header =
  let open Angstrom in
  match boundary with
  | None ->
    let write_line line = emitter (Some (line ^ "\n")) in
    let write_data data = emitter (Some data) in
    (match Header.content_encoding header with
     | `Quoted_printable -> Quoted_printable.to_end_of_input ~write_line ~write_data
     | `Base64 -> B64.to_end_of_input ~write_data
     | `Bit7 | `Bit8 | `Binary -> to_end_of_input ~write_data
     | `Ietf_token _ | `X_token _ -> assert false) >>= fun () ->
    emitter None ; return ()
  | Some boundary ->
    let end_of_body = Rfc2046.make_delimiter boundary in
    match Header.content_encoding header with
    | `Quoted_printable ->
      Quoted_printable.with_emitter ~emitter end_of_body
      >>= fun () -> emitter None ; return ()
    | `Base64 ->
      B64.with_emitter ~emitter end_of_body
      >>= fun () -> emitter None ; return ()
    | `Bit7 | `Bit8 | `Binary ->
      with_emitter ~emitter end_of_body
      >>= fun () -> emitter None ; return ()
    | `Ietf_token _ | `X_token _ -> assert false

let boundary header =
  let content_type = Header.content_type header in
  match List.assoc_opt "boundary" (Content_type.parameters content_type) with
  | Some (`Token boundary) | Some (`String boundary) -> Some boundary
  | None -> None

(* Literally the hard part of [mrmime]. You need to know that inside a mail, we
   can have a [`Multipart] (a list of bodies) but a [`Message] too. *)
let mail =
  let open Angstrom in
  let rec body parent header =
    match Content_type.ty (Header.content_type header) with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Content_type.Type.discrete -> heavy_octet parent header >>| fun body -> Leaf { header; body; }
    | `Message ->
      mail parent >>| fun (header', body') -> Message { header= header'; body= body' }
    | `Multipart ->
      match boundary header with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map snd
        >>| fun parts -> Multipart { header; body= parts; }
      | None -> fail "expected boundary"

  and mail parent =
    Header.Decoder.header <* char '\r' <* char '\n'
    >>= fun header -> match Content_type.ty (Header.content_type header) with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Content_type.Type.discrete ->
      heavy_octet parent header >>| fun body ->
      header, Leaf { header; body; }
    | `Message ->
      mail parent >>| fun (header', message') ->
      header, Message { header= header'; body= message' }
    | `Multipart ->
      match boundary header with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map snd
        >>| fun parts -> header, Multipart { header; body= parts; }
      | None -> fail "expected boundary" in

  mail None

type 'id emitters = Header.t -> (string option -> unit) * 'id

let stream
  : emitters:'id emitters -> (Header.t * 'id t) Angstrom.t
  = fun ~emitters ->
  let open Angstrom in
  let rec body parent header =
    match Content_type.ty (Header.content_type header) with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Content_type.Type.discrete ->
      let emitter, id = emitters header in
      light_octet ~emitter parent header >>| fun () -> Leaf { header; body= id; }
    | `Message ->
      mail parent >>| fun (header', body') -> Message { header= header'; body= body'; }
    | `Multipart ->
      match boundary header with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map (fun (_header, body) -> body)
        >>| fun parts -> Multipart { header; body= parts; }
      | None -> fail "expected boundary"

  and mail parent =
    Header.Decoder.header <* char '\r' <* char '\n' >>= fun header ->
    match Content_type.ty (Header.content_type header) with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Content_type.Type.discrete ->
      let emitter, id = emitters header in
      light_octet ~emitter parent header
      >>| fun () -> header, Leaf { header; body= id; }
    | `Message ->
      mail parent >>| fun (header', body') ->
      header, Message { header= header'; body= body'; }
    | `Multipart ->
      match boundary header with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map (fun (_header, body) -> body)
        >>| fun parts -> header, Multipart { header; body= parts; }
      | None -> fail "expected boundary" in

  mail None
