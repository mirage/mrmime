module Field = struct
  type mail = [ Rfc5322.field | Rfc5322.resent | Rfc5322.trace | Rfc2045.field | Rfc2045.field_version | Rfc5322.unsafe | Rfc5322.lines ]
  type part = [ Rfc5322.field | Rfc5322.resent | Rfc5322.trace | Rfc2045.field | Rfc5322.unsafe | Rfc5322.lines ]
end

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
  ; fields : (Number.t * Field.part * Location.t) list
  ; part : ('discrete, 'extension) part option }

type garbage =
  [ `Unsafe of Field_name.t * Unstructured.t
  | `Lines of (string * Location.t) list ]

open Angstrom

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

let header : (Content.t * Header.t * Resent.t * Trace.t * Garbage.t) Angstrom.t =
  let nothing_to_do _ = fail "Nothing to do" in
  Rfc5322.header (Rfc2045.message_field nothing_to_do nothing_to_do)
  >>| List.mapi (fun i (field, loc) -> Number.of_int_exn i, field, loc)
  >>= fun fields -> return (Resent.reduce fields Resent.empty)
  >>= fun (resents, _) -> return (Trace.reduce fields Trace.empty)
  >>= fun (traces, _) -> return (Content.reduce_as_mail fields Content.empty)
  >>= fun (content, _) -> return (Header.reduce (rfc5322 fields) Header.empty)
  >>= fun (header, fields) -> return (clean fields)
  >>= function
  | [] -> return (content, header, resents, traces, Garbage.empty)
  | rest -> return (content, header, resents, traces, Garbage.make rest)

type ('valid, 'invalid) contents =
  | Contents of 'valid
  | Invalid of 'invalid

let contents x = Contents x
let invalid x = Invalid x

let best_effort strict_parser raw_parser =
  ((strict_parser >>| fun () -> `Contents)
   <|> (raw_parser >>| fun () -> `Invalid))

let octet boundary content =
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

let boundary content =
  match Content_type.Parameters.find "boundary" (Content.parameters content) with
  | Some (`Token boundary) | Some (`String boundary) -> Some boundary
  | None -> None

(* Literally the hard part of [mrmime]. You need to know that inside a mail, we
   can have a [`Multipart] (a list of bodies) but a [`Message] too. *)
let mail =
  let rec body parent content _fields =
    match Content.ty content with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Rfc2045.discrete -> octet parent content >>| fun body -> Part_discrete body
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
      octet parent content
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
