type 'a t =
  | Leaf of 'a
  | Multipart of (Header.t * 'a t option) list
  | Message of Header.t * 'a t

let rec index v max idx chr =
  if idx >= max then raise Not_found;
  if Bstr.get v idx = chr then idx else index v max (succ idx) chr

let index v chr = index v (Bstr.length v) 0 chr

let parser ~write_data end_of_body =
  let open Angstrom in
  let check_end_of_body =
    let len = String.length end_of_body in
    Unsafe.peek len Bstr.sub_string >>| String.equal end_of_body
  in

  fix @@ fun m ->
  let choose chunk = function
    | true ->
        let chunk = Bytes.sub_string chunk 0 (Bytes.length chunk - 1) in
        write_data chunk;
        commit
    | false ->
        write_data (Bytes.unsafe_to_string chunk);
        advance 1 *> commit *> m
  in

  available >>= function
  | 0 -> peek_char *> m
  | len -> (
      Unsafe.peek len Bstr.sub >>= fun chunk ->
      match index chunk end_of_body.[0] with
      | pos ->
          let tmp = Bytes.create (pos + 1) in
          Bstr.blit_to_bytes chunk ~src_off:0 tmp ~dst_off:0 ~len:(pos + 1);
          advance pos *> check_end_of_body <* commit >>= choose tmp
      | exception Not_found ->
          write_data (Bstr.to_string chunk);
          advance len *> commit *> m)

let with_buffer end_of_body =
  let buf = Buffer.create 0x100 in
  let write_data = Buffer.add_string buf in
  let open Angstrom in
  parser ~write_data end_of_body >>| fun () -> Buffer.contents buf

let with_emitter ~emitter end_of_body =
  let write_data x = emitter (Some x) in
  parser ~write_data end_of_body

let to_end_of_input ~write_data =
  let open Angstrom in
  fix @@ fun m ->
  peek_char >>= function
  | None -> commit
  | Some _ ->
      available >>= fun n ->
      Unsafe.take n (fun ba ~off ~len ->
          let chunk = Bytes.create len in
          Bstr.blit_to_bytes ba ~src_off:off chunk ~dst_off:0 ~len;
          write_data (Bytes.unsafe_to_string chunk))
      >>= fun () -> m

let heavy_octet ?(transfer_encoding = true) boundary header =
  let open Angstrom in
  match boundary with
  | None ->
      let buf = Buffer.create 0x800 in
      let write_line line =
        Buffer.add_string buf line;
        Buffer.add_string buf "\n"
      in
      let write_data = Buffer.add_string buf in
      (match Header.content_encoding header with
        | `Quoted_printable ->
            if transfer_encoding then
              Quoted_printable.to_end_of_input ~write_data ~write_line
            else to_end_of_input ~write_data
        | `Base64 ->
            if transfer_encoding then B64.to_end_of_input ~write_data
            else to_end_of_input ~write_data
        | `Bit7 | `Bit8 | `Binary -> to_end_of_input ~write_data
        | `Ietf_token _x | `X_token _x -> to_end_of_input ~write_data)
      >>| fun () -> Buffer.contents buf
  | Some boundary -> (
      let end_of_body = Rfc2046.make_delimiter boundary in
      match Header.content_encoding header with
      | `Quoted_printable ->
          if transfer_encoding then Quoted_printable.with_buffer end_of_body
          else with_buffer end_of_body
      | `Base64 ->
          if transfer_encoding then B64.with_buffer end_of_body
          else with_buffer end_of_body
      | `Bit7 | `Bit8 | `Binary -> with_buffer end_of_body
      | `Ietf_token _x | `X_token _x -> with_buffer end_of_body)

let light_octet ?(transfer_encoding = true) ~emitter boundary header =
  let open Angstrom in
  match boundary with
  | None ->
      let write_line line = emitter (Some (line ^ "\n")) in
      let write_data data = emitter (Some data) in
      (match Header.content_encoding header with
        | `Quoted_printable ->
            if transfer_encoding then
              Quoted_printable.to_end_of_input ~write_line ~write_data
            else to_end_of_input ~write_data
        | `Base64 ->
            if transfer_encoding then B64.to_end_of_input ~write_data
            else to_end_of_input ~write_data
        | `Bit7 | `Bit8 | `Binary -> to_end_of_input ~write_data
        | `Ietf_token _ | `X_token _ -> to_end_of_input ~write_data)
      >>= fun () ->
      emitter None;
      return ()
  | Some boundary -> (
      let end_of_body = Rfc2046.make_delimiter boundary in
      match Header.content_encoding header with
      | `Quoted_printable when transfer_encoding ->
          Quoted_printable.with_emitter ~emitter end_of_body >>= fun () ->
          emitter None;
          return ()
      | `Base64 when transfer_encoding ->
          B64.with_emitter ~emitter end_of_body >>= fun () ->
          emitter None;
          return ()
      | _ ->
          with_emitter ~emitter end_of_body >>= fun () ->
          emitter None;
          return ())

let boundary header =
  let content_type = Header.content_type header in
  match Content_type.boundary content_type with
  | Some _ as value -> value
  | None -> None

(*
      let { Content_type.parameters; _ } = content_type in
      Logs.debug (fun m ->
          m "Content-Type parameters: @[<hov>%a@]"
            Fmt.(Dump.list (Dump.pair string Content_type.Parameters.pp_value))
            parameters);
      None
*)

(* Literally the hard part of [mrmime]. You need to know that inside a mail, we
   can have a [`Multipart] (a list of bodies) but a [`Message] too. *)
let mail ?transfer_encoding g =
  let open Angstrom in
  let rec body parent header =
    match Content_type.ty (Header.content_type header) with
    | #Content_type.Type.discrete | `Ietf_token _ | `X_token _ ->
        heavy_octet ?transfer_encoding parent header >>| fun body -> Leaf body
    | `Message ->
        mail parent >>| fun (header', body') -> Message (header', body')
    | `Multipart -> (
        match boundary header with
        | Some boundary ->
            Rfc2046.multipart_body ?parent boundary
              (body (Option.some boundary))
            >>| fun parts -> Multipart parts
        | None -> fail "expected boundary")
  and mail parent =
    Header.Decoder.header g <* char '\r' <* char '\n' >>= fun header ->
    match Content_type.ty (Header.content_type header) with
    | #Content_type.Type.discrete | `Ietf_token _ | `X_token _ ->
        heavy_octet ?transfer_encoding parent header >>| fun body ->
        (header, Leaf body)
    | `Message ->
        mail parent >>| fun (header', message') ->
        (header, Message (header', message'))
    | `Multipart -> (
        match boundary header with
        | Some boundary ->
            Rfc2046.multipart_body ?parent boundary
              (body (Option.some boundary))
            >>| fun parts -> (header, Multipart parts)
        | None -> fail "expected boundary")
  in
  mail None

type 'id emitters = Header.t -> (string option -> unit) * 'id

let stream :
    ?transfer_encoding:bool ->
    ?g:Field.witness Field_name.Map.t ->
    'id emitters ->
    (Header.t * 'id t) Angstrom.t =
 fun ?transfer_encoding ?g emitters ->
  let open Angstrom in
  let rec body parent header =
    match Content_type.ty (Header.content_type header) with
    | #Content_type.Type.discrete | `Ietf_token _ | `X_token _ ->
        let emitter, id = emitters header in
        light_octet ?transfer_encoding ~emitter parent header >>| fun () ->
        Leaf id
    | `Message ->
        mail parent >>| fun (header', body') -> Message (header', body')
    | `Multipart -> (
        match boundary header with
        | Some boundary ->
            Rfc2046.multipart_body ?parent boundary
              (body (Option.some boundary))
            >>| fun parts -> Multipart parts
        | None -> fail "expected boundary")
  and mail parent =
    Header.Decoder.header g <* char '\r' <* char '\n' >>= fun header ->
    match Content_type.ty (Header.content_type header) with
    | #Content_type.Type.discrete | `Ietf_token _ | `X_token _ ->
        let emitter, id = emitters header in
        light_octet ?transfer_encoding ~emitter parent header >>| fun () ->
        (header, Leaf id)
    | `Message ->
        mail parent >>| fun (header', body') ->
        (header, Message (header', body'))
    | `Multipart -> (
        match boundary header with
        | Some boundary ->
            Rfc2046.multipart_body ?parent boundary
              (body (Option.some boundary))
            >>| fun parts -> (header, Multipart parts)
        | None -> fail "expected boundary")
  in

  mail None
