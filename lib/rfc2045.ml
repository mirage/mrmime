type discrete = [`Text | `Image | `Audio | `Video | `Application]
type composite = [`Message | `Multipart]
type extension = [`Ietf_token of string | `X_token of string]
type ty = [discrete | composite | extension]

type subty =
  [`Ietf_token of string | `Iana_token of string | `X_token of string]

type mechanism =
  [ `Bit7
  | `Bit8
  | `Binary
  | `QuotedPrintable
  | `Base64
  | `Ietf_token of string
  | `X_token of string ]

type value = [`String of string | `Token of string]
type content = {ty: ty; subty: subty; parameters: (string * value) list}
type version = int * int

type field =
  [ `ContentType of content
  | `ContentEncoding of mechanism
  | `ContentID of Rfc822.msg_id
  | `ContentDescription of Rfc5322.unstructured
  | `Content of string * Rfc5322.unstructured ]

type unsafe = [`Unsafe of string * Rfc5322.unstructured]
type skip = [`Skip of string list]
type field_version = [`MimeVersion of version]

open Angstrom

let of_string s a =
  match parse_string a s with Ok v -> Some v | Error _ -> None

let is_tspecials = function
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
   |']' | '?' | '=' ->
      true
  | _ -> false

let invalid_token token = Fmt.kstrf fail "invalid token: %s" token
let nothing_to_do = Fmt.kstrf fail "nothing to do"

(* / *)

let is_ctl = function '\000' .. '\031' -> true | _ -> false
let is_space = ( = ) ' '
let is_token c = (not (is_tspecials c)) && (not (is_ctl c)) && not (is_space c)
let is_digit = function '0' .. '9' -> true | _ -> false
let token = take_while1 is_token
let attribute = token >>| String.lowercase_ascii
let ietf_token = token
let iana_token = token

let x_token =
  satisfy (function 'x' | 'X' -> true | _ -> false) *> char '-' *> token

let extension_token =
  peek_char
  >>= function
  | Some 'X' | Some 'x' -> x_token >>| fun v -> `X_token v
  | _ -> ietf_token >>| fun v -> `Ietf_token v

let composite_ty =
  token
  >>= fun s ->
  (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
  match String.lowercase_ascii s with
  | "message" -> return `Message
  | "multipart" -> return `Multipart
  | _ -> (
    match of_string s extension_token with
    | Some v -> return v
    | None -> invalid_token s )

let ty =
  token
  >>= fun s ->
  (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
  match String.lowercase_ascii s with
  | "text" -> return `Text
  | "image" -> return `Image
  | "audio" -> return `Audio
  | "video" -> return `Video
  | "application" -> return `Application
  | "message" -> return `Message
  | "multipart" -> return `Multipart
  | _ -> (
    match of_string s extension_token with
    | Some v -> return v
    | None -> invalid_token s )

let ty_to_string = function
  | `Text -> "text"
  | `Image -> "image"
  | `Audio -> "audio"
  | `Video -> "video"
  | `Application -> "application"
  | `Message -> "message"
  | `Multipart -> "multipart"
  | `Ietf_token s | `X_token s -> s

let subty ty =
  peek_char
  >>= (function
        | Some 'X' | Some 'x' -> extension_token
        | _ -> (
            token
            >>| fun v ->
            try
              `Iana_token
                (Iana.Set.find v (Iana.Map.find (ty_to_string ty) Iana.iana))
            with _exn -> `X_token v ))
  >>| fun subty -> (ty, subty)

let value =
  Rfc822.quoted_string
  >>| (fun v -> `String v)
  <|> (token >>| fun v -> `Token v)

let parameter =
  attribute
  >>= fun attribute -> char '=' *> value >>| fun value -> (attribute, value)

let content =
  option () Rfc822.cfws *> ty
  <* option () Rfc822.cfws
  <* char '/'
  <* option () Rfc822.cfws
  >>= subty
  <* option () Rfc822.cfws
  >>= fun (ty, subty) ->
  many (char ';' *> option () Rfc822.cfws *> parameter)
  >>| fun parameters -> {ty; subty; parameters}

let version =
  option () Rfc822.cfws *> take_while1 is_digit
  >>| int_of_string
  <* option () Rfc822.cfws
  <* char '.'
  <* option () Rfc822.cfws
  >>= fun a ->
  take_while1 is_digit
  >>| int_of_string
  <* option () Rfc822.cfws
  >>| fun b -> (a, b)

let mechanism =
  token
  >>= fun s ->
  (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
  match String.lowercase_ascii s with
  | "7bit" -> return `Bit7
  | "8bit" -> return `Bit8
  | "binary" -> return `Binary
  | "quoted-printable" -> return `QuotedPrintable
  | "base64" -> return `Base64
  | _ -> (
    match of_string s extension_token with
    | Some v -> return v
    | None -> invalid_token s )

let encoding = option () Rfc822.cfws *> mechanism <* option () Rfc822.cfws
let id = option () Rfc822.cfws *> Rfc5321.msg_id <* option () Rfc822.cfws

let field extend_mime extend field_name =
  (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
  match String.lowercase_ascii field_name with
  | "content-type" -> content <* Rfc822.crlf >>| fun v -> `ContentType v
  | "content-transfer-encoding" ->
      encoding <* Rfc822.crlf >>| fun v -> `ContentEncoding v
  | "content-id" -> id <* Rfc822.crlf >>| fun v -> `ContentID v
  | "content-description" ->
      Rfc5322.unstructured <* Rfc822.crlf >>| fun v -> `ContentDescription v
  | _ ->
      if
        String.length field_name >= String.length "Content-"
        && String.lowercase_ascii
           @@ String.sub field_name 0 (String.length "Content-")
           = "content-"
      then
        let field_name =
          String.sub field_name (String.length "Content-")
            (String.length field_name - String.length "Content-")
        in
        extend_mime field_name
        <|> ( Rfc5322.unstructured
            <* Rfc822.crlf
            >>| fun v -> `Content (field_name, v) )
      else extend field_name

let sp = Format.sprintf

let part_field extend_mime extend field_name =
  field extend_mime extend field_name
  <|> ( Rfc5322.unstructured
      <* Rfc822.crlf
      >>| (fun v -> `Unsafe (field_name, v))
      <?> sp "Unsafe %s" field_name )

let message_field extend_mime extend field_name =
  field extend_mime
    (fun field_name ->
      (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
      match String.lowercase_ascii field_name with
      | "mime-version" -> version <* Rfc822.crlf >>| fun v -> `MimeVersion v
      | _ -> extend field_name )
    field_name

let entity_part_headers extend_mime extend =
  many
    ( Rfc5322.field_name
    <* many (satisfy (function '\x09' | '\x20' -> true | _ -> false))
    <* char ':'
    >>= (fun field_name -> part_field extend_mime extend field_name)
    <|> (Rfc5322.skip_field >>| fun lines -> `Skip lines) )

let entity_message_headers extend_mime extend =
  many
    ( Rfc5322.field_name
    <* many (satisfy (function '\x09' | '\x20' -> true | _ -> false))
    <* char ':'
    >>= fun field_name -> message_field extend_mime extend field_name )

let mime_message_headers extend_mime extend =
  entity_message_headers extend_mime (fun field_name ->
      (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
      match String.lowercase_ascii field_name with
      | "mime-version" -> version <* Rfc822.crlf >>| fun v -> `MimeVersion v
      | _ -> extend field_name )

let mime_part_headers extend =
  entity_part_headers (fun _ -> nothing_to_do) extend
