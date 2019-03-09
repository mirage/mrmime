(* Note that RFC 2047 comes from RFC 1521, RFC 1522m RFC 1590 and RFC 1342.
   Purpose of its RFC is to able to able to use encoding (like ISO-8859-1) when
   only 8bit is accepted by SMTP and utf8 is accepted by eSMTP.

   We handle only ASCII and [Rosetta.encoding] and normalize them to utf8 with
   [Uutf]. Otherwise, we keep data as is and let the user to normalize it to
   utf8. *)

type uutf_charset = [`UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE]

type charset =
  [Rosetta.encoding | uutf_charset | `US_ASCII | `Charset of string]

type encoding = Quoted_printable | Base64

type encoded_word =
  { charset: charset
  ; encoding: encoding
  ; raw: string
  ; data: (string, Rresult.R.msg) result}

let is_normalized {charset; _} =
  match charset with `Charset _ -> false | _ -> true

let charset_of_uppercase_string x =
  match String.uppercase_ascii x with
  | "US-ASCII" | "iso-ir-6" | "ANSI_X3.4-1968" | "ANSI_X3.4-1986"
   |"ISO_646.rv:1991" | "ISO646-US" | "us" | "IBM367" | "cp367" | "csASCII" ->
      `US_ASCII
  | x -> (
    try (Rosetta.encoding_of_string x :> charset)
    with Invalid_argument _ -> (
      match Uutf.encoding_of_string x with
      | Some (#uutf_charset as charset) -> charset
      | _ -> `Charset x ) )

let charset_of_string = function
  | "US-ASCII" | "iso-ir-6" | "ANSI_X3.4-1968" | "ANSI_X3.4-1986"
   |"ISO_646.rv:1991" | "ISO646-US" | "us" | "IBM367" | "cp367" | "csASCII" ->
      `US_ASCII
  | x -> (
    try (Rosetta.encoding_of_string x :> charset)
    with Invalid_argument _ -> (
      match Uutf.encoding_of_string x with
      | Some (#uutf_charset as charset) -> charset
      | _ -> charset_of_uppercase_string x ) )

open Angstrom

(* From RFC 2047

        especials = "(" / ")" / "<" / ">" / "@" / "," / ";" / ":" / "
                    <"> / "/" / "[" / "]" / "?" / "." / "="
*)
let is_especials = function
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '"' | '/' | '[' | ']'
   |'?' | '.' | '=' ->
      true
  | _ -> false

let is_ctl = function '\000' .. '\031' -> true | _ -> false
let is_space = ( = ) ' '

(* From RFC 2047

        token = 1*<Any CHAR except SPACE, CTLs, and especials>
*)
let token =
  take_while1 (fun chr -> not (is_especials chr || is_ctl chr || is_space chr))

type share = [`End | `Uchar of Uchar.t]

let normalize_quoted_printable_with_rosetta ?(chunk = 512) ~charset raw =
  let tmp = Bytes.create chunk in
  let res = Buffer.create chunk in
  let pos = ref 0 in
  let qp_decoder = Pecu.Inline.decoder (`String raw) in
  let decoder = Rosetta.decoder charset `Manual in
  let encoder = Uutf.encoder `UTF_8 (`Buffer res) in
  let rec go_encode v =
    match (v, Uutf.encode encoder v) with
    | `End, `Ok -> `Ok
    | _, `Ok -> go_decode ()
    | _, `Partial -> assert false
  (* XXX(dinosaure): [Uutf.encoder_dst encoder <> `Manual] *)
  and go_decode () =
    match Rosetta.decode decoder with
    | `Await -> go_qp_decode ()
    | #share as v -> go_encode v
    | `Malformed _ as v -> v
  and go_qp_decode () =
    match Pecu.Inline.decode qp_decoder with
    | `Await ->
        assert false
        (* XXX(dinosaure): [Pecu.Inline.decoder_src qp_decoder <> `Manual]. *)
    | `Char chr ->
        Bytes.unsafe_set tmp !pos chr ;
        if !pos + 1 = chunk then (
          pos := 0 ;
          Rosetta.src decoder tmp 0 chunk )
        else incr pos ;
        go_decode ()
    | `End ->
        (let i = !pos in
         pos := 0 ;
         Rosetta.src decoder tmp 0 i) ;
        go_decode ()
    | `Malformed _ as v -> v
  in
  match go_qp_decode () with
  | `Ok -> Ok (Buffer.contents res)
  | `Malformed data -> Rresult.R.error_msgf "Malformed input: %S" data

let normalize_quoted_printable_with_uutf ?(chunk = 512) ~charset raw =
  let tmp = Bytes.create chunk in
  let res = Buffer.create chunk in
  let pos = ref 0 in
  let qp_decoder = Pecu.Inline.decoder (`String raw) in
  let decoder = Uutf.decoder ~encoding:charset `Manual in
  let encoder = Uutf.encoder `UTF_8 (`Buffer res) in
  let rec go_encode v =
    match (v, Uutf.encode encoder v) with
    | `End, `Ok -> `Ok
    | _, `Ok -> go_decode ()
    | _, `Partial -> assert false
  (* XXX(dinosaure): [Uutf.encoder_dst encoder <> `Manual] *)
  and go_decode () =
    match Uutf.decode decoder with
    | `Await -> go_qp_decode ()
    | #share as v -> go_encode v
    | `Malformed _ as v -> v
  and go_qp_decode () =
    match Pecu.Inline.decode qp_decoder with
    | `Await ->
        assert false
        (* XXX(dinosaure): [Pecu.Inline.decoder_src qp_decoder <> `Manual]. *)
    | `Char chr ->
        Bytes.unsafe_set tmp !pos chr ;
        if !pos + 1 = chunk then (
          pos := 0 ;
          Uutf.Manual.src decoder tmp 0 chunk )
        else incr pos ;
        go_decode ()
    | `End ->
        (let i = !pos in
         pos := 0 ;
         Uutf.Manual.src decoder tmp 0 i) ;
        go_decode ()
    | `Malformed _ as v -> v
  in
  match go_qp_decode () with
  | `Ok -> Ok (Buffer.contents res)
  | `Malformed data -> Rresult.R.error_msgf "Malformed input: %S" data

let normalize_base64_with_rosetta ?(chunk = 512) ~charset raw =
  let res = Buffer.create chunk in
  match Base64.decode raw with
  | Error _ as err -> err
  | Ok decoded ->
    let decoder = Rosetta.decoder charset (`String decoded) in
    let encoder = Uutf.encoder `UTF_8 (`Buffer res) in
    let rec go_encode v =
      match (v, Uutf.encode encoder v) with
      | `End, `Ok -> `Ok
      | _, `Ok -> go_decode ()
      | _, `Partial -> assert false
    (* XXX(dinosaure): [Uutf.encoder_dst encoder <> `Manual] *)
    and go_decode () =
      match Rosetta.decode decoder with
      | `Await ->
        assert false
      (* XXX(dinosaure): [Rosetta.decoder_src decoder <> `Manual] *)
      | #share as v -> go_encode v
      | `Malformed _ as v -> v
    in
    match go_decode () with
    | `Ok -> Ok (Buffer.contents res)
    | `Malformed data -> Rresult.R.error_msgf "Malformed input: %S" data

let normalize_base64_with_uutf ?(chunk = 512) ~charset raw =
  let res = Buffer.create chunk in
  match Base64.decode raw with
  | Error _ as err -> err
  | Ok decoded ->
    let decoder = Uutf.decoder ~encoding:charset (`String decoded) in
    let encoder = Uutf.encoder `UTF_8 (`Buffer res) in
    let rec go_encode v =
      match (v, Uutf.encode encoder v) with
      | `End, `Ok -> `Ok
      | _, `Ok -> go_decode ()
      | _, `Partial -> assert false
    (* XXX(dinosaure): [Uutf.encoder_dst encoder <> `Manual] *)
    and go_decode () =
      match Uutf.decode decoder with
      | `Await ->
        assert false
      (* XXX(dinosaure): [Rosetta.decoder_src decoder <> `Manual] *)
      | #share as v -> go_encode v
      | `Malformed _ as v -> v
    in
    match go_decode () with
    | `Ok -> Ok (Buffer.contents res)
    | `Malformed data -> Rresult.R.error_msgf "Malformed input: %S" data

let normalize_quoted_printable ?(chunk = 512) ~charset raw =
  match charset with
  | `US_ASCII -> (
      let res = Buffer.create chunk in
      let qp_decoder = Pecu.Inline.decoder (`String raw) in
      let encoder = Uutf.encoder `UTF_8 (`Buffer res) in
      let rec go_encode v =
        match (v, Uutf.encode encoder v) with
        | `End, `Ok -> `Ok
        | _, `Ok -> go_qp_decode ()
        | _, `Partial -> assert false
      and go_qp_decode () =
        match Pecu.Inline.decode qp_decoder with
        | `Await -> assert false
        | `Char chr -> go_encode (`Uchar (Uchar.of_char chr))
        | `End -> go_encode `End
        | `Malformed _ as v -> v
      in
      match go_qp_decode () with
      | `Ok -> Ok (Buffer.contents res)
      | `Malformed data -> Rresult.R.error_msgf "Malformed input: %S" data )
  | #uutf_charset as charset ->
      normalize_quoted_printable_with_uutf ~chunk ~charset raw
  | #Rosetta.encoding as charset ->
      normalize_quoted_printable_with_rosetta ~chunk ~charset raw
  | `Charset _ -> (
      let res = Buffer.create chunk in
      let qp_decoder = Pecu.Inline.decoder (`String raw) in
      let rec go_qp_decode () =
        match Pecu.Inline.decode qp_decoder with
        | `Await -> assert false
        | `Char chr -> Buffer.add_char res chr ; go_qp_decode ()
        | `End -> `Ok
        | `Malformed _ as v -> v
      in
      match go_qp_decode () with
      | `Ok -> Ok (Buffer.contents res)
      | `Malformed data -> Rresult.R.error_msgf "Malformed input: %S" data )

let normalize_base64 ?chunk ~charset raw =
  match charset with
  | `US_ASCII ->
      (* XXX(dinosaure): UTF-8 is a superset of ASCII. Then, we probably need to
       check if characters are between '\000' and '\127' but it's probably ok.
       paranoid mode or not? TODO! *)
      Base64.decode raw
  | #uutf_charset as charset -> normalize_base64_with_uutf ?chunk ~charset raw
  | #Rosetta.encoding as charset ->
      normalize_base64_with_rosetta ?chunk ~charset raw
  | `Charset _ -> Base64.decode raw

let normalize ?chunk ~charset ~encoding raw =
  match encoding with
  | Quoted_printable -> normalize_quoted_printable ?chunk ~charset raw
  | Base64 -> normalize_base64 ?chunk ~charset raw

let invalid_encoding = Fmt.kstrf fail "Invalid encoding '%c'"

(* From RFC 2047

        encoded-text = 1*<Any printable ASCII character other than "?"
                          or SPACE>
                       ; (but see "Use of encoded-words in message
                       ; headers", section 5)
*)
let encoded_text = take_while1 (function '?' | ' ' -> false | _ -> true)

(* From RFC 2047

        encoded-word = "=?" charset "?" encoding "?" encoded-text "?="
        charset = token    ; see section 3
        encoding = token   ; see section 4

      Both 'encoding' and 'charset' names are case-independent.  Thus the
      charset name "ISO-8859-1" is equivalent to "iso-8859-1", and the
      encoding named "Q" may be spelled either "Q" or "q".

    About charset

      The 'charset' portion of an 'encoded-word' specifies the character
      set associated with the unencoded text.  A 'charset' can be any of
      the character set names allowed in an MIME "charset" parameter of a
      "text/plain" body part, or any character set name registered with
      IANA for use with the MIME text/plain content-type.

    About encoding

      Initially, the legal values for "encoding" are "Q" and "B".  These
      encodings are described below.  The "Q" encoding is recommended for
      use when most of the characters to be encoded are in the ASCII
      character set; otherwise, the "B" encoding should be used.
      Nevertheless, a mail reader which claims to recognize 'encoded-word's
      MUST be able to accept either encoding for any character set which it
      supports.

   See [pecu], [rosetta] and [ocaml-base64] for more details about encoding.
*)
let encoded_word =
  string "=?" *> token
  >>| charset_of_string
  >>= fun charset ->
  char '?' *> satisfy (function 'Q' | 'q' | 'B' | 'b' -> true | _ -> false)
  >>= (function
        | 'Q' | 'q' -> return Quoted_printable
        | 'B' | 'b' -> return Base64
        | encoding -> invalid_encoding encoding)
  >>= fun encoding ->
  char '?' *> encoded_text
  >>= fun raw -> return (normalize ~chunk:512 ~charset ~encoding raw)
  >>= fun data -> string "?=" *> return {charset; encoding; raw; data}
