module Field = struct
  type mail = [ Rfc5322.field | Rfc2045.field | Rfc2045.field_version | Rfc5322.skip ]
  type part = [ Rfc5322.field | Rfc2045.field | Rfc5322.skip ]
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

let header : (Content.t * Header.t * Field.mail list) Angstrom.t =
  Rfc5322.header
    (Rfc2045.message_field
       (fun _ -> fail "Nothing to do")
       (fun _ -> fail "Nothing to do"))
  >>= fun fields -> return (Header.fold fields Header.default)
  >>= fun (header, fields) -> return (Content.fold_as_mail fields Content.default)
  >>= fun (content, fields) -> return (content, header, fields)

let octet boundary content =
  match boundary with
  | None -> assert false
  | Some boundary ->
    let end_of_body = Rfc2046.make_delimiter boundary in
    match content.Content.encoding with
    | `Quoted_printable -> Quoted_printable.with_buffer end_of_body
    | `Base64 -> Base64.with_buffer end_of_body
    | `Bit7 | `Bit8 | `Binary -> Rfc5322.with_buffer end_of_body
    | `Ietf_token _x | `X_token _x -> assert false

let boundary content =
  match List.assoc "boundary" content.Content.ty.Rfc2045.parameters with
  | `Token boundary | `String boundary -> Some boundary
  | exception Not_found -> None

(* Literally the hard part of [mrmime]. You need to know that inside a mail, we
   can have a [`Multipart] (a list of bodies) but a [`Message] too. *)
let mail =
  let rec body parent content _fields =
    match content.Content.ty.Rfc2045.ty with
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
    >>= fun (content, header, fields) -> match content.Content.ty.Rfc2045.ty with
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
