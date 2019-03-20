(* Note that RFC 2045 comes from RFC 1521, RFC 1341 and RFC 1049. An update, RFC
   2231, is not implemented yet. About utf8 and [String.lowercase_ascii], RFCs
   are not clear about that - if it's possible to use utf8 inside [Content-Type]
   field-value. *)

type discrete = [`Text | `Image | `Audio | `Video | `Application]
type composite = [`Message | `Multipart]
type extension = [`Ietf_token of string | `X_token of string]

(* In general, the top-level media type is used to declare the general type of
   data.

   If another top-level type is to be used for any reason, it must be given a
   name starting with ["X-"] to indicate its non-standard status and to avoid a
   potential conflict with a future official name. *)
type ty = [discrete | composite | extension]

(* While the subtype specifies a specific format for that type of data. Thus, a
   media type of ["image/xyz"] is enough to tell a user agent that the data is
   an image, even if the user agent has no knowledge of the specific image
   format ["xyz"]. Such information can be used, for example, to decide whether
   or not to show a user the raw data from an unrecognized subtype -- such an
   action might be reasonable for unrecognized subtype of text, but not for
   unrecognized subtypes of image or audio.

   For this reason, registered subtypes of text, image, audio and video should
   not contain embedded information that is really of a different type. Such
   compound formats should be represented using the ["multipart"] or
   ["application"] types. *)
type subty =
  [`Ietf_token of string | `Iana_token of string | `X_token of string]

type mechanism =
  [ `Bit7
  | `Bit8
  | `Binary
  | `Quoted_printable
  | `Base64
  | `Ietf_token of string
  | `X_token of string ]

(* Parameters are modifiers of the media subtype, and as such do not
   fundamentally alter the nature of the content. The set of meaningful
   parameters depends on the media type and subtype, However, a given top-level
   media type may define parameters which are applicable to any subtype of that
   type.

   Parameters may be required by their defining content type or subtype or they
   may be optional. TypeBeat does not check any assumption about parameters
   except for the [`Multipart] where it expects the ["boundary"] parameter - but
   TypeBeat does not process (not yet!) the ["charset"] parameter for example.

   Note that the value of a parameter can be a quoted string ([`String]). In
   this case, the value does not include the quotes. That is, the quotation
   marks in a quoted-string are not a part of the value of the parameter, but
   are merely used to delimit that parameter value. In other case, the value is
   [`Token]. *)
type value = [`String of string | `Token of string]
type content = {ty: ty; subty: subty; parameters: (string * value) list}
type version = int * int

type field =
  [ `ContentType of content
  | `ContentEncoding of mechanism
  | `ContentID of Rfc822.nonsense Rfc822.msg_id
  | `ContentDescription of Rfc5322.unstructured
  | `Content of string * Rfc5322.unstructured ]

type unsafe = [`Unsafe of string * Rfc5322.unstructured]
type lines = [`Lines of (string * Location.t) list]
type field_version = [`MIMEVersion of version]

open Angstrom

let of_string s a =
  match parse_string a s with Ok v -> Some v | Error _ -> None

(* From RFC 2045

        tspecials :=  "(" / ")" / "<" / ">" / "@" /
                      "," / ";" / ":" / "\" / <">
                      "/" / "[" / "]" / "?" / "="
                      ; Must be in quoted-string,
                      ; to use within parameter values

      Note that the definition of "tspecials" is the same as the RFC 822
      definition of "specials" with the addition of the three characters
      "/", "?", and "=", and the removal of ".".
*)
let is_tspecials = function
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
   |']' | '?' | '=' ->
      true
  | _ -> false

let invalid_token token = Fmt.kstrf fail "invalid token: %s" token
let nothing_to_do = Fmt.kstrf fail "nothing to do"

(* / *)

let is_ctl = function '\000' .. '\031' | '\127' -> true | _ -> false
let is_space = ( = ) ' '

(* From RFC 2045

        token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
                    or tspecials>
*)
let is_ascii = function '\000' .. '\127' -> true | _ -> false
let is_token c = (is_ascii c) && (not (is_tspecials c)) && (not (is_ctl c)) && (not (is_space c))
let token = take_while1 is_token
let is_digit = function '0' .. '9' -> true | _ -> false

(* From RFC 2045

        attribute := token
                     ; Matching of attributes
                     ; is ALWAYS case-insensitive.
*)
let attribute = token >>| String.lowercase_ascii

(* From RFC 2045

        ietf-token := <An extension token defined by a
                          standards-track RFC and registered
                          with IANA.>
        iana-token := <A publicly-defined extension token. Tokens
                          of this form must be registered with IANA
                          as specified in RFC 2048.>

   XXX(dinosaure): we don't check at this time if IETF/IANA token exists.
*)
let ietf_token = token
let iana_token = token

(* From RFC 2045

        x-token := <The two characters "X-" or "x-" followed, with
                       no intervening white space, by any token>
*)
let x_token =
  satisfy (function 'x' | 'X' -> true | _ -> false) *> char '-' *> token

(* From RFC 2045

        extension-token := ietf-token / x-token
*)
let extension_token =
  peek_char
  >>= function
  | Some 'X' | Some 'x' -> x_token >>| fun v -> `X_token v
  | _ -> ietf_token >>| fun v -> `Ietf_token v

(* From RFC 2045

        composite-type := "message" / "multipart" / extension-token
*)
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

(* From RFC 2045

        discrete-type := "text" / "image" / "audio" / "video" /
                         "application" / extension-token
        composite-type := "message" / "multipart" / extension-token
        type := discrete-type / composite-type
*)
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

(* From RFC 2045

        subtype := extension-token / iana-token
*)
let subty ty =
  token
  >>= fun s ->
    try let v = `Iana_token (Iana.Set.find s (Iana.Map.find (ty_to_string ty) Iana.database)) in return (ty, v)
    with Not_found -> match of_string s extension_token with
      | Some v -> return (ty, v)
      | None -> invalid_token s

(* From RFC 2045

        value := token / quoted-string
*)
let value =
  Rfc822.quoted_string
  >>| (fun v -> `String v)
  <|> (token >>| fun v -> `Token v)

(* From RFC 2045

        parameter := attribute "=" value
*)
let parameter =
  attribute
  >>= fun attribute -> char '=' *> value >>| fun value -> (attribute, value)

(* From RFC 2045

        content := "Content-Type" ":" type "/" subtype
                   *(";" parameter)
                   ; Matching of media type and subtype
                   ; is ALWAYS case-insensitive.

   XXX(dinosaure): As others fields on mails, we consider CFWS between
   tokens as RFC 822 said:

        Each header field can be viewed as a single, logical  line  of
        ASCII  characters,  comprising  a field-name and a field-body.
        For convenience, the field-body  portion  of  this  conceptual
        entity  can be split into a multiple-line representation; this
        is called "folding".  The general rule is that wherever  there
        may  be  linear-white-space  (NOT  simply  LWSP-chars), a CRLF
        immediately followed by AT LEAST one LWSP-char may instead  be
        inserted.
*)
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

(* From RFC 2045

        version := "MIME-Version" ":" 1*DIGIT "." 1*DIGIT

   XXX(dinosaure): RFC 2045 does not talk about CFWS token between number.
     However, if I put it, it's may be because some mails use the folding
     whitespace in this field value.
*)
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

(* From RFC 2045

        mechanism := "7bit" / "8bit" / "binary" /
                     "quoted-printable" / "base64" /
                     ietf-token / x-token

      These values are not case sensitive -- Base64 and BASE64 and bAsE64
      are all equivalent.  An encoding type of 7BIT requires that the body
      is already in a 7bit mail-ready representation.  This is the default
      value -- that is, "Content-Transfer-Encoding: 7BIT" is assumed if the
      Content-Transfer-Encoding header field is not present.
*)
let mechanism =
  token
  >>= fun s ->
  (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
  match String.lowercase_ascii s with
  | "7bit" -> return `Bit7
  | "8bit" -> return `Bit8
  | "binary" -> return `Binary
  | "quoted-printable" -> return `Quoted_printable
  | "base64" -> return `Base64
  | _ -> (
    match of_string s extension_token with
    | Some v -> return v
    | None -> invalid_token s )

(* From RFC 2045

        encoding := "Content-Transfer-Encoding" ":" mechanism
*)
let encoding = option () Rfc822.cfws *> mechanism <* option () Rfc822.cfws

(* From RFC 2045

        id := "Content-ID" ":" msg-id

   XXX(dinosaure): RFC 2045 appears before RFC 5321. By this way,
   try to handle [IPv4]/[IPv6] or [Ext] (describe in RFC 5321) does
   not make sense at this stage - we raise an ["Invalid domain"].
*)
let id = option () Rfc822.cfws *> Rfc822.msg_id ~address_literal:(fail "Invalid domain") <* option () Rfc822.cfws

let with_location p =
  pos >>= fun a -> p >>= fun r -> pos >>= fun b -> return (r, Location.make a b)

(* From RFC 5322

   This specification uses the Augmented Backus-Naur Form (ABNF)
   [RFC5234] notation for the formal definitions of the syntax of
   messages.  Characters will be specified either by a decimal value
   (e.g., the value %d65 for uppercase A and %d97 for lowercase A) or by
   a case-insensitive literal value enclosed in quotation marks (e.g.,
   "A" for either uppercase or lowercase A).

   From RFC 2045

   See {!content}, {!encoding}, {!id} and

        description := "Content-Description" ":" *text

   Field is defined by *case-insensitive* literal value. We can use
   [String.lowercase_ascii].

   Internationalization of email headers (RFC6532) say:

        Note that this protocol does not change rules in RFC 5322 for
        defining header field names. ... but the header field names themselves
        must consist of ASCII characters only.

   *ascii* part of [String.lowercase_ascii] stills valid.
*)
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

let sp = Fmt.strf

(* From RFC 2045

        MIME-part-headers := entity-headers
                             [fields]
                             ; Any field not beginning with
                             ; "content-" can have no defined
                             ; meaning and may be ignored.
                             ; The ordering of the header
                             ; fields implied by this BNF
                             ; definition should be ignored.
*)
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
      | "mime-version" -> version <* Rfc822.crlf >>| fun v -> `MIMEVersion v
      | _ -> extend field_name )
    field_name

let entity_part_headers extend_mime extend =
  let p =
    ( Rfc5322.field_name
    <* many (satisfy (function '\x09' | '\x20' -> true | _ -> false))
    <* char ':'
    >>= (fun field_name -> part_field extend_mime extend field_name)
    <|> (Rfc5322.lines >>| fun lines -> `Lines lines) ) in
  many (with_location p)

let entity_message_headers extend_mime extend =
  let p =
    ( Rfc5322.field_name
    <* many (satisfy (function '\x09' | '\x20' -> true | _ -> false))
    <* char ':'
    >>= fun field_name -> message_field extend_mime extend field_name ) in
  many (with_location p)

(* From RFC 2045

        MIME-message-headers := entity-headers
                                fields
                                version CRLF
                                ; The ordering of the header
                                ; fields implied by this BNF
                                ; definition should be ignored.
*)
let mime_message_headers extend_mime extend =
  entity_message_headers extend_mime (fun field_name ->
      (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
      match String.lowercase_ascii field_name with
      | "mime-version" -> version <* Rfc822.crlf >>| fun v -> `MIMEVersion v
      | _ -> extend field_name )

(* From RFC 2045

        MIME-part-headers := entity-headers
                             [fields]
                             ; Any field not beginning with
                             ; "content-" can have no defined
                             ; meaning and may be ignored.
                             ; The ordering of the header
                             ; fields implied by this BNF
                             ; definition should be ignored.
*)
let mime_part_headers extend =
  entity_part_headers (fun _ -> nothing_to_do) extend
