module Field = struct
  type mail = [ Rfc5322.field | Rfc2045.field | Rfc2045.field_version | Rfc5322.lines ]
  type part = [ Rfc5322.field | Rfc2045.field | Rfc5322.lines ]
end

type ('discrete, 'extension) t =
  | Discrete of { content : Content.t
                ; fields : Field.mail list
                ; body : 'discrete }
  | Extension of { content : Content.t
                 ; fields : Field.mail list
                 ; body : 'extension }
  | Multipart of { content : Content.t
                 ; fields : Field.mail list
                 ; parts : ('discrete, 'extension) atom list }
  | Message of { content : Content.t
               ; header : Header.t
               ; fields : Field.mail list
               ; message : ('discrete, 'extension) t }
and ('discrete, 'extension) part =
  | Part_discrete of 'discrete
  | Part_extension of 'extension
  | Part_multipart of ('discrete, 'extension) atom list
  | Part_message of { header : Header.t
                    ; message : ('discrete, 'extension) t }
and ('discrete, 'extension) atom =
  { content : Content.t
  ; fields : Field.part list
  ; part : ('discrete, 'extension) part option }

open Angstrom

let header : (Content.t * Header.t * (Number.t * Field.mail) list) Angstrom.t =
  Rfc5322.header
    (Rfc2045.message_field
       (fun _ -> fail "Nothing to do")
       (fun _ -> fail "Nothing to do"))
  >>= fun fields -> return (Header.fold Number.zero fields Header.default)
  >>= fun (header, fields) ->
  let fields = List.map (fun (idx, field, _) -> (idx, field)) fields in
  return (Content.fold_as_mail fields Content.default)
  >>= fun (content, fields) -> return (content, header, fields)

let octet boundary content =
  match boundary with
  | None ->
    let buf = Buffer.create 0x800 in
    let write_line line =
      Buffer.add_string buf line ;
      Buffer.add_string buf "\n" in
    let write_data = Buffer.add_string buf in
    (match Content.encoding content with
     | `Quoted_printable -> Quoted_printable.to_end_of_input ~write_data ~write_line
     | `Base64 -> B64.to_end_of_input ~write_data
     | `Bit7 | `Bit8 | `Binary -> Rfc5322.to_end_of_input ~write_data
     | `Ietf_token _x | `X_token _x -> assert false) >>| fun () -> Buffer.contents buf
  | Some boundary ->
    let end_of_body = Rfc2046.make_delimiter boundary in
    match Content.encoding content with
    | `Quoted_printable -> Quoted_printable.with_buffer end_of_body
    | `Base64 -> B64.with_buffer end_of_body
    | `Bit7 | `Bit8 | `Binary -> Rfc5322.with_buffer end_of_body
    | `Ietf_token _x | `X_token _x -> assert false

let boundary content =
  match List.assoc "boundary" (Content.parameters content) with
  | `Token boundary | `String boundary -> Some boundary
  | exception Not_found -> None

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
        >>| List.map (fun (content, fields, part) -> { content; fields= List.map snd fields; part; })
        >>| fun parts -> Part_multipart parts
      | None -> fail "expected boundary"

  and mail parent =
    header <* Rfc822.crlf
    >>= fun (content, header, fields) ->
    match Content.ty content with
    | `Ietf_token _x | `X_token _x -> assert false
    | #Rfc2045.discrete ->
      octet parent content
      >>| fun body -> header, Discrete { content; fields= List.map snd fields; body; }
    | `Message ->
      mail parent >>| fun (header', message') -> header, Message { content; fields= List.map snd fields; header= header'; message= message' }
    | `Multipart ->
      match boundary content with
      | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map (fun (content, fields, part) -> { content; fields= List.map snd fields; part; })
        >>| fun parts -> header, Multipart { content; fields= List.map snd fields; parts; }
      | None -> fail "expected boundary" in

  mail None
