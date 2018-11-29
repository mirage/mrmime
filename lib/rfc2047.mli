type encoded_string =
  {charset: charset; encoding: encoding; data: (string, Rresult.R.msg) result}

and charset = [Rosetta.encoding | `ASCII | `Charset of string]

and encoding = Quoted_printable | Base64

val is_normalized : encoded_string -> bool

open Angstrom

val is_ctl : char -> bool
val is_space : char -> bool
val token : string t
val inline_encoded_string : encoded_string t
