type charset = [Rosetta.encoding | `ASCII | `Charset of string]
type encoding = Quoted_printable | Base64

type encoded_string =
  {charset: charset; encoding: encoding; data: (string, Rresult.R.msg) result}

let is_normalized {charset; _} =
  match charset with `Charset _ -> false | _ -> true

let charset_of_string = function
  | "US-ASCII" | "iso-ir-6" | "ANSI_X3.4-1968" | "ANSI_X3.4-1986"
   |"ISO_646.rv:1991" | "ISO646-US" | "us" | "IBM367" | "cp367" | "csASCII" ->
      `ASCII
  | x -> (
    try (Rosetta.encoding_of_string x :> charset) with Invalid_argument _ ->
      `Charset x )

open Angstrom

let is_especials = function
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '"' | '/' | '[' | ']'
   |'?' | '.' | '=' ->
      true
  | _ -> false

let is_ctl = function '\000' .. '\031' -> true | _ -> false
let is_space = ( = ) ' '

let token =
  take_while1 (fun chr -> not (is_especials chr || is_ctl chr || is_space chr))

type share = [`End | `Uchar of Uchar.t]

let normalize_quoted_printable ?(chunk = 512) ~charset raw =
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

let normalize_base64 ?(chunk = 512) ~charset raw =
  let res = Buffer.create chunk in
  let decoded = B64.decode raw in
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

let normalize_quoted_printable ?(chunk = 512) ~charset raw =
  match charset with
  | `ASCII -> (
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
  | #Rosetta.encoding as charset ->
      normalize_quoted_printable ~chunk ~charset raw
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
  | `ASCII ->
      (* XXX(dinosaure): UTF-8 is a superset of ASCII. *) Ok (B64.decode raw)
  | #Rosetta.encoding as charset -> normalize_base64 ?chunk ~charset raw
  | `Charset _ -> Ok (B64.decode raw)

let normalize ?chunk ~charset ~encoding raw =
  match encoding with
  | Quoted_printable -> normalize_quoted_printable ?chunk ~charset raw
  | Base64 -> normalize_base64 ?chunk ~charset raw

let invalid_encoding = Fmt.kstrf fail "Invalid encoding '%c'"

let inline_encoded_string =
  string "=?" *> token
  >>| charset_of_string
  >>= fun charset ->
  char '?' *> satisfy (function 'Q' | 'B' -> true | _ -> false)
  >>= (function
        | 'Q' -> return Quoted_printable
        | 'B' -> return Base64
        | encoding -> invalid_encoding encoding)
  >>= fun encoding ->
  char '?' *> take_while (( <> ) '?')
  >>| normalize ~chunk:512 ~charset ~encoding
  >>= fun data -> string "?=" *> return {charset; encoding; data}
