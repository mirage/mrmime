type field_mail = [ Rfc5322.field | Rfc5322.resent | Rfc5322.trace | Rfc2045.field | Rfc2045.field_version | Rfc5322.unsafe | Rfc5322.lines ]
type field_part = [ Rfc5322.field | Rfc5322.resent | Rfc5322.trace | Rfc2045.field | Rfc5322.unsafe | Rfc5322.lines ]

type ('discrete, 'extension) t =
  | Discrete of { content : Content.t
                ; fields : Garbage.t
                ; body : 'discrete }
  | Extension of { content : Content.t
                 ; fields : Garbage.t
                 ; body : 'extension }
  | Multipart of { content : Content.t
                 ; fields : Garbage.t
                 ; parts : ('discrete, 'extension) atom list }
  | Message of { content : Content.t
               ; header : Header.t
               ; fields : Garbage.t
               ; message : ('discrete, 'extension) t }
and ('discrete, 'extension) part =
  | Part_discrete of 'discrete
  | Part_extension of 'extension
  | Part_multipart of ('discrete, 'extension) atom list
  | Part_message of { header : Header.t
                    ; message : ('discrete, 'extension) t }
and ('discrete, 'extension) atom =
  { content : Content.t
  ; fields : (Number.t * field_part * Location.t) list
  ; part : ('discrete, 'extension) part option }

type garbage =
  [ `Unsafe of Field_name.t * Unstructured.t
  | `Lines of (string * Location.t) list ]

let rfc5322 fields =
  let go acc = function
    | _, #Rfc5322.resent, _ -> acc
    | _, #Rfc5322.trace, _ -> acc
    | _, #Rfc2045.field, _ -> acc
    | _, #Rfc2045.field_version, _ -> acc
    | _, #Rfc5322.field, _ as x -> x :: acc
    | _, #garbage, _ as x -> x :: acc in
  List.fold_left go [] fields |> List.rev

let clean fields =
  let go acc = function
    | _, #Rfc5322.field, _ -> acc
    | _, #garbage, _ as x -> x :: acc in
  List.fold_left go [] fields |> List.rev

type ('valid, 'invalid) contents =
  | Contents of 'valid
  | Invalid of 'invalid

type heavy_t = ((string, string) contents, string) t
type 'id light_t = ('id, 'id) t

open Angstrom

let header : (Content.t * Header.t * Resent.t list * Trace.t list * Garbage.t) Angstrom.t =
  let nothing_to_do _ = fail "Nothing to do" in
  Rfc5322.header (Rfc2045.message_field nothing_to_do nothing_to_do)
  >>| List.mapi (fun i (field, loc) -> Number.of_int_exn i, field, loc)
  >>= fun fields -> return (Resent.reduce fields [])
  >>= fun (resents, _) -> return (Trace.reduce fields [])
  >>= fun (traces, _) -> return (Content.reduce_as_mail fields Content.empty)
  >>= fun (content, _) -> return (Header.reduce (rfc5322 fields) Header.empty)
  >>= fun (header, fields) -> return (clean fields)
  >>= function
  | [] -> return (content, header, resents, traces, Garbage.empty)
  | rest -> return (content, header, resents, traces, Garbage.make rest)

let contents x = Contents x
let invalid x = Invalid x

let best_effort strict_parser raw_parser =
  ((strict_parser >>| fun () -> `Contents)
   <|> (raw_parser >>| fun () -> `Invalid))

let heavy_octet boundary content =
  match boundary with
  | None ->
    let buf = Buffer.create 0x800 in
    let write_line line =
      Buffer.add_string buf line ;
      Buffer.add_string buf "\n" in
    let write_data = Buffer.add_string buf in
    (match Content.encoding content with
     | `Quoted_printable ->
       best_effort
         (Quoted_printable.to_end_of_input ~write_data ~write_line)
         ((return (Buffer.clear buf)) *> Rfc5322.to_end_of_input ~write_data)
     | `Base64 ->
       best_effort
         (B64.to_end_of_input ~write_data)
         ((return (Buffer.clear buf)) *> Rfc5322.to_end_of_input ~write_data)
     | `Bit7 | `Bit8 | `Binary ->
       (Rfc5322.to_end_of_input ~write_data >>| fun () -> `Contents)
     | `Ietf_token _x | `X_token _x -> assert false) >>|
    (function
      | `Contents -> Contents (Buffer.contents buf)
      | `Invalid -> Invalid (Buffer.contents buf))
  | Some boundary ->
    let end_of_body = Rfc2046.make_delimiter boundary in
    match Content.encoding content with
    | `Quoted_printable ->
      choice
        [ (Quoted_printable.with_buffer end_of_body >>| contents)
        ; (Rfc5322.with_buffer end_of_body >>| invalid) ]
    | `Base64 ->
      choice
        [ (B64.with_buffer end_of_body >>| contents)
        ; (Rfc5322.with_buffer end_of_body >>| invalid) ]
    | `Bit7 | `Bit8 | `Binary -> (Rfc5322.with_buffer end_of_body >>| contents)
    | `Ietf_token _x | `X_token _x -> assert false

let light_octet ~emitter boundary content =
  match boundary with
  | None ->
    let write_line line = emitter (Some (line ^ "\n")) in
    let write_data data = emitter (Some data) in
    (match Content.encoding content with
     | `Quoted_printable -> Quoted_printable.to_end_of_input ~write_line ~write_data
     | `Base64 -> B64.to_end_of_input ~write_data
     | `Bit7 | `Bit8 | `Binary -> Rfc5322.to_end_of_input ~write_data
     | `Ietf_token _ | `X_token _ -> assert false) >>= fun () ->
    emitter None ; return ()
  | Some boundary ->
    let end_of_body = Rfc2046.make_delimiter boundary in
    match Content.encoding content with
    | `Quoted_printable ->
      Quoted_printable.with_emitter ~emitter end_of_body
      >>= fun () -> emitter None ; return ()
    | `Base64 ->
      B64.with_emitter ~emitter end_of_body
      >>= fun () -> emitter None ; return ()
    | `Bit7 | `Bit8 | `Binary ->
      Rfc5322.with_emitter ~emitter end_of_body
      >>= fun () -> emitter None ; return ()
    | `Ietf_token _ | `X_token _ -> assert false

let boundary content =
  match Content_type.Parameters.find "boundary" (Content.parameters content) with
  | Some (`Token boundary) | Some (`String boundary) -> Some boundary
  | None -> None

(* Literally the hard part of [mrmime]. You need to know that inside a mail, we
   can have a [`Multipart] (a list of bodies) but a [`Message] too. *)
let mail : (Header.t * heavy_t) Angstrom.t =
  let rec body parent content _fields =
    match Content.ty content with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Rfc2045.discrete -> heavy_octet parent content >>| fun body -> Part_discrete body
    | `Message ->
      mail parent
      >>| fun (header', body') -> Part_message { header= header'; message= body' }
    | `Multipart ->
      match boundary content with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map (fun (content, fields, part) -> { content; fields; part; })
        >>| fun parts -> Part_multipart parts
      | None -> fail "expected boundary"

  and mail parent =
    header <* Rfc822.crlf
    >>= fun (content, header, resents, traces, fields) ->
    match Content.ty content with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Rfc2045.discrete ->
      heavy_octet parent content
      >>| fun body -> header, Discrete { content; fields; body; }
    | `Message ->
      mail parent >>| fun (header', message') -> header, Message { content; fields; header= header'; message= message' }
    | `Multipart ->
      match boundary content with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map (fun (content, fields, part) -> { content; fields; part; })
        >>| fun parts -> header, Multipart { content; fields; parts; }
      | None -> fail "expected boundary" in

  mail None

type 'id emitters = Content.t -> (string option -> unit) * 'id

let light_mail
  : emitters:'id emitters -> (Header.t * 'id light_t) Angstrom.t
  = fun ~emitters ->
  let rec body parent content _fields =
    match Content.ty content with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Rfc2045.discrete ->
      let emitter, id = emitters content in
      light_octet ~emitter parent content >>| fun () -> Part_discrete id
    | `Message ->
      mail parent
      >>| fun (header', body') -> Part_message { header= header'; message= body' }
    | `Multipart ->
      match boundary content with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map (fun (content, fields, part) -> { content; fields; part; })
        >>| fun parts -> Part_multipart parts
      | None -> fail "expected boundary"

  and mail parent =
    header <* Rfc822.crlf
    >>= fun (content, header, resents, traces, fields) ->
    match Content.ty content with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Rfc2045.discrete ->
      let emitter, id = emitters content in
      light_octet ~emitter parent content
      >>| fun () -> header, Discrete { content; fields; body= id; }
    | `Message ->
      mail parent >>| fun (header', message') -> header, Message { content; fields; header= header'; message= message' }
    | `Multipart ->
      match boundary content with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map (fun (content, fields, part) -> { content; fields; part; })
        >>| fun parts -> header, Multipart { content; fields; parts; }
      | None -> fail "expected boundary" in

  mail None
