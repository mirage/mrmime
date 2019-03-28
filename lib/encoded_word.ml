type uutf_charset = [`UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE]
type charset = [Rosetta.encoding | uutf_charset | `US_ASCII | `Charset of string]
type encoding = Rfc2047.encoding = Quoted_printable | Base64
type t = Rfc2047.encoded_word =
  { charset: charset
  ; encoding: encoding
  ; raw: string * char * string
  ; data: (string, Rresult.R.msg) result}

exception Invalid_utf8

let b = Rfc2047.Base64
let q = Rfc2047.Quoted_printable

let is_utf8_valid_string x =
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function `Malformed _ -> raise Invalid_utf8 | _ -> ())
      () x ;
    true
  with Invalid_utf8 -> false

let is_normalized = Rfc2047.is_normalized

let make ~encoding value =
  if is_utf8_valid_string value then
    Some {Rfc2047.charset= `UTF_8; encoding; raw= "", '\000', "" (* TODO *); data= Ok value}
  else None

let make_exn ~encoding value =
  match make ~encoding value with
  | Some v -> v
  | None -> Fmt.invalid_arg "make_exn: invalid encoded-word"

let encoding {Rfc2047.encoding; _} = encoding
let charset {Rfc2047.charset; _} = charset
let data {Rfc2047.data; _} = data

(* XXX(dinosaure): used by encoder, no fancy box or whatever. *)
let pp_charset ppf = function
  | #Rfc2047.uutf_charset as encoding ->
      Fmt.string ppf (Uutf.encoding_to_string encoding)
  | #Rosetta.encoding as encoding ->
      Fmt.string ppf (Rosetta.encoding_to_string encoding)
  | `US_ASCII -> Fmt.string ppf "US-ASCII"
  | `Charset encoding -> Fmt.string ppf encoding

let pp_encoding ppf = function
  | Rfc2047.Base64 -> Fmt.string ppf "base64"
  | Rfc2047.Quoted_printable -> Fmt.string ppf "quoted-printable"

let pp ppf t =
  Fmt.pf ppf "{ @[<hov>charset = %a;@ encoding = %a;@ data = %a;@] }"
    pp_charset t.Rfc2047.charset pp_encoding t.Rfc2047.encoding
    Fmt.(Dump.result ~ok:Fmt.string ~error:Rresult.R.pp_msg)
    t.Rfc2047.data

let equal_charset a b = (Pervasives.( = ) : charset -> charset -> bool) a b
let equal_encoding a b = (Pervasives.( = ) : encoding -> encoding -> bool) a b

let equal a b =
  equal_charset a.Rfc2047.charset b.Rfc2047.charset
  && equal_encoding a.Rfc2047.encoding b.Rfc2047.encoding
  && Rresult.R.equal ~ok:String.equal
       ~error:(fun (`Msg _) (`Msg _) -> true)
       a.Rfc2047.data b.Rfc2047.data

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let encoding ppf = function
    | Base64 -> char ppf 'B'
    | Quoted_printable -> char ppf 'Q'

  let charset = using (Fmt.to_to_string pp_charset) string

  let to_quoted_printable input =
    let buffer = Buffer.create (String.length input) in
    let encoder = Pecu.Inline.encoder (`Buffer buffer) in
    String.iter (fun chr -> ignore @@ Pecu.Inline.encode encoder (`Char chr)) input ;
    ignore @@ Pecu.Inline.encode encoder `End ;
    Buffer.contents buffer

  let quoted_printable = using to_quoted_printable string
  let base64 = using (fun x -> Base64.encode_exn ~pad:true x) string

  let is_base64 = function
    | Base64 -> true | _ -> false

  let encoded_word ppf t =
    match t.Rfc2047.data with
    | Ok data ->
      let format = [ hov 0; string $ "=?"; !!charset; char $ '?'; !!encoding; char $ '?'; a; string $ "?="; close ] in
      let data_format = if is_base64 t.Rfc2047.encoding then base64 else quoted_printable in
      keval ppf id format t.Rfc2047.charset t.Rfc2047.encoding data_format data
    | Error (`Msg err) ->
      Fmt.invalid_arg "Impossible to encode an invalid encoded-word: %s" err
end
