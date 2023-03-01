type uutf_charset = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]

type charset =
  [ Rosetta.encoding | uutf_charset | `US_ASCII | `Charset of string ]

type encoding = Quoted_printable | Base64

type t =
  { charset : charset;
    encoding : encoding;
    data : (string, [ `Msg of string ]) result
  }

exception Invalid_utf8

let b = Base64
let q = Quoted_printable
let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let is_utf8_valid_string x =
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function `Malformed _ -> raise Invalid_utf8 | _ -> ())
      () x;
    true
  with Invalid_utf8 -> false

let is_normalized { charset; _ } =
  match charset with `Charset _ -> false | _ -> true

let normalize_to_utf8 ~charset raw =
  match charset with
  | `US_ASCII -> Ok raw (* XXX(dinosaure): UTF-8 is a superset of US-ASCII *)
  | #Rosetta.encoding as charset ->
      let bf = Buffer.create (String.length raw) in
      let decoder = Rosetta.decoder charset (`String raw) in
      let encoder = Uutf.encoder `UTF_8 (`Buffer bf) in
      let rec go () =
        match Rosetta.decode decoder with
        | `Malformed err -> error_msgf "%s" err
        | `Await -> assert false
        | `Uchar _ as uchar ->
            let[@warning "-8"] (`Ok : [ `Ok | `Partial ]) =
              Uutf.encode encoder uchar
            in
            go ()
        | `End as v ->
            let[@warning "-8"] (`Ok : [ `Ok | `Partial ]) =
              Uutf.encode encoder v
            in
            Ok (Buffer.contents bf)
      in
      go ()
  | `UTF_8 -> Ok raw (* XXX(dinosaure): check? *)
  | #uutf_charset as charset ->
      let bf = Buffer.create (String.length raw) in
      let decoder = Uutf.decoder ~encoding:charset (`String raw) in
      let encoder = Uutf.encoder `UTF_8 (`Buffer bf) in
      let rec go () =
        match Uutf.decode decoder with
        | `Malformed err -> error_msgf "%s" err
        | `Await -> assert false
        | `Uchar _ as uchar ->
            let[@warning "-8"] (`Ok : [ `Ok | `Partial ]) =
              Uutf.encode encoder uchar
            in
            go ()
        | `End as v ->
            let[@warning "-8"] (`Ok : [ `Ok | `Partial ]) =
              Uutf.encode encoder v
            in
            Ok (Buffer.contents bf)
      in
      go ()
  | `Charset v -> error_msgf "encoding %s is not supported" v

let make ~encoding value =
  if is_utf8_valid_string value then
    Ok { charset = `UTF_8; encoding; data = Ok value }
  else error_msgf "%S is not a valid UTF-8 string" value

let make_exn ~encoding value =
  match make ~encoding value with
  | Ok v -> v
  | Error (`Msg err) -> invalid_arg "%s" err

let encoding { encoding; _ } = encoding
let charset { charset; _ } = charset
let data { data; _ } = data

(* XXX(dinosaure): used by encoder, no fancy box or whatever. *)
let pp_charset ppf = function
  | #uutf_charset as encoding ->
      Format.pp_print_string ppf (Uutf.encoding_to_string encoding)
  | #Rosetta.encoding as encoding ->
      Format.pp_print_string ppf (Rosetta.encoding_to_string encoding)
  | `US_ASCII -> Format.pp_print_string ppf "US-ASCII"
  | `Charset encoding -> Format.pp_print_string ppf encoding

let pp_encoding ppf = function
  | Base64 -> Format.pp_print_string ppf "base64"
  | Quoted_printable -> Format.pp_print_string ppf "quoted-printable"

let pp ppf t =
  Format.fprintf ppf "{ @[<hov>charset = %a;@ encoding = %a;@ data = %a;@] }"
    pp_charset t.charset pp_encoding t.encoding
    (Format.pp_print_result ~ok:Format.pp_print_string
       ~error:(fun ppf (`Msg msg) -> Format.pp_print_string ppf msg))
    t.data

let equal_charset a b = (Stdlib.( = ) : charset -> charset -> bool) a b
let equal_encoding a b = (Stdlib.( = ) : encoding -> encoding -> bool) a b

let equal a b =
  equal_charset a.charset b.charset
  && equal_encoding a.encoding b.encoding
  && Result.equal ~ok:String.equal
       ~error:(fun (`Msg _) (`Msg _) -> true) (* XXX(dinosaure): or [false]? *)
       a.data b.data

let charset_of_uppercase_string x =
  match String.uppercase_ascii x with
  | "US-ASCII" | "iso-ir-6" | "ANSI_X3.4-1968" | "ANSI_X3.4-1986"
  | "ISO_646.rv:1991" | "ISO646-US" | "us" | "IBM367" | "cp367" | "csASCII" ->
      `US_ASCII
  | x -> (
      try (Rosetta.encoding_of_string x :> charset)
      with Invalid_argument _ -> (
        match Uutf.encoding_of_string x with
        | Some (#uutf_charset as charset) -> charset
        | _ -> `Charset x))

let charset_of_string x =
  match x with
  | "US-ASCII" | "iso-ir-6" | "ANSI_X3.4-1968" | "ANSI_X3.4-1986"
  | "ISO_646.rv:1991" | "ISO646-US" | "us" | "IBM367" | "cp367" | "csASCII" ->
      `US_ASCII
  | x -> (
      try (Rosetta.encoding_of_string x :> charset)
      with Invalid_argument _ -> (
        match Uutf.encoding_of_string x with
        | Some (#uutf_charset as charset) -> charset
        | _ -> charset_of_uppercase_string x))

let charset_to_string = Format.asprintf "%a" pp_charset

module Decoder = struct
  open Angstrom

  (* From RFC 2047

          especials = "(" / ")" / "<" / ">" / "@" / "," / ";" / ":" / "
                      <"> / "/" / "[" / "]" / "?" / "." / "="
  *)
  let is_especials = function
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '"' | '/' | '[' | ']'
    | '?' | '.' | '=' ->
        true
    | _ -> false

  let is_ctl = function '\000' .. '\031' -> true | _ -> false
  let is_space = ( = ) ' '

  (* From RFC 2047

          token = 1*<Any CHAR except SPACE, CTLs, and especials>
  *)
  let token =
    take_while1 (fun chr ->
        not (is_especials chr || is_ctl chr || is_space chr))

  type end_or_uchar = [ `End | `Uchar of Uchar.t ]

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
      | #end_or_uchar as v -> go_encode v
      | `Malformed _ as v -> v
    and go_qp_decode () =
      match Pecu.Inline.decode qp_decoder with
      | `Await ->
          assert false
          (* XXX(dinosaure): [Pecu.Inline.decoder_src qp_decoder <> `Manual]. *)
      | `Char chr ->
          Bytes.unsafe_set tmp !pos chr;
          if !pos + 1 = chunk then (
            pos := 0;
            Rosetta.src decoder tmp 0 chunk)
          else incr pos;
          go_decode ()
      | `End ->
          (let i = !pos in
           pos := 0;
           Rosetta.src decoder tmp 0 i);
          go_decode ()
      | `Malformed _ as v -> v
    in
    match go_qp_decode () with
    | `Ok -> Ok (Buffer.contents res)
    | `Malformed data -> error_msgf "Malformed input: %S" data

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
      | #end_or_uchar as v -> go_encode v
      | `Malformed _ as v -> v
    and go_qp_decode () =
      match Pecu.Inline.decode qp_decoder with
      | `Await ->
          assert false
          (* XXX(dinosaure): [Pecu.Inline.decoder_src qp_decoder <> `Manual]. *)
      | `Char chr ->
          Bytes.unsafe_set tmp !pos chr;
          if !pos + 1 = chunk then (
            pos := 0;
            Uutf.Manual.src decoder tmp 0 chunk)
          else incr pos;
          go_decode ()
      | `End ->
          (let i = !pos in
           pos := 0;
           Uutf.Manual.src decoder tmp 0 i);
          go_decode ()
      | `Malformed _ as v -> v
    in
    match go_qp_decode () with
    | `Ok -> Ok (Buffer.contents res)
    | `Malformed data -> error_msgf "Malformed input: %S" data

  let normalize_base64_with_rosetta ?(chunk = 512) ~charset raw =
    let res = Buffer.create chunk in
    match Base64.decode raw with
    | Error _ as err -> err
    | Ok decoded -> (
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
          | #end_or_uchar as v -> go_encode v
          | `Malformed _ as v -> v
        in
        match go_decode () with
        | `Ok -> Ok (Buffer.contents res)
        | `Malformed data -> error_msgf "Malformed input: %S" data)

  let normalize_base64_with_uutf ?(chunk = 512) ~charset raw =
    let res = Buffer.create chunk in
    match Base64.decode raw with
    | Error _ as err -> err
    | Ok decoded -> (
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
          | #end_or_uchar as v -> go_encode v
          | `Malformed _ as v -> v
        in
        match go_decode () with
        | `Ok -> Ok (Buffer.contents res)
        | `Malformed data -> error_msgf "Malformed input: %S" data)

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
        | `Malformed data -> error_msgf "Malformed input: %S" data)
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
          | `Char chr ->
              Buffer.add_char res chr;
              go_qp_decode ()
          | `End -> `Ok
          | `Malformed _ as v -> v
        in
        match go_qp_decode () with
        | `Ok -> Ok (Buffer.contents res)
        | `Malformed data -> error_msgf "Malformed input: %S" data)

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

  let invalid_encoding = Format.kasprintf fail "Invalid encoding '%c'"

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
    string "=?" *> token >>| charset_of_string >>= fun charset ->
    char '?' *> satisfy (function 'Q' | 'q' | 'B' | 'b' -> true | _ -> false)
    >>= fun encoding_raw ->
    (match encoding_raw with
    | 'Q' | 'q' -> return Quoted_printable
    | 'B' | 'b' -> return Base64
    | encoding -> invalid_encoding encoding)
    >>= fun encoding ->
    char '?' *> encoded_text >>= fun raw ->
    return (normalize ~chunk:512 ~charset ~encoding raw) >>= fun data ->
    string "?=" *> return { charset; encoding; data }
end

let of_string x =
  match
    Angstrom.parse_string ~consume:Angstrom.Consume.Prefix Decoder.encoded_word
      x
  with
  | Ok v -> Ok v
  | Error _ -> error_msgf "%S is not a valid encoded-word" x

module Encoder = struct
  open Prettym

  let encoding ppf = function
    | Base64 -> char ppf 'B'
    | Quoted_printable -> char ppf 'Q'

  let charset = using (Format.asprintf "%a" pp_charset) string

  let to_quoted_printable input =
    let buffer = Stdlib.Buffer.create (String.length input) in
    let encoder = Pecu.Inline.encoder (`Buffer buffer) in
    String.iter
      (fun chr -> ignore @@ Pecu.Inline.encode encoder (`Char chr))
      input;
    ignore @@ Pecu.Inline.encode encoder `End;
    Stdlib.Buffer.contents buffer

  let quoted_printable = using to_quoted_printable string
  let base64 = using (fun x -> Base64.encode_exn ~pad:true x) string
  let is_base64 = function Base64 -> true | _ -> false

  let encoded_word ppf t =
    match t.data with
    | Ok data ->
        let fmt =
          [ bbox;
            string $ "=?";
            !!charset;
            char $ '?';
            !!encoding;
            char $ '?';
            a;
            string $ "?=";
            close
          ]
        in
        let encoder =
          if is_base64 t.encoding then base64 else quoted_printable
        in
        eval ppf fmt t.charset t.encoding encoder data
    | Error (`Msg err) ->
        invalid_arg "Impossible to encode an invalid encoded-word: %s" err
end
