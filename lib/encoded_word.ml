type charset = Rfc2047.charset
type encoding = Rfc2047.encoding = Quoted_printable | Base64
type t = Rfc2047.encoded_word

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
    Some {Rfc2047.charset= `UTF_8; encoding; data= Ok value}
  else None

let encoding {Rfc2047.encoding; _} = encoding
let charset {Rfc2047.charset; _} = charset
let data {Rfc2047.data; _} = data

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
