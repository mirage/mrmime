type charset =
  [ `UTF_8
  | `UTF_16
  | `UTF_16BE
  | `UTF_16LE
  | Rosetta.encoding
  | `US_ASCII
  | `Charset of string ]

type encoding = Rfc2047.encoding = Quoted_printable | Base64

val b : encoding
val q : encoding

type t = Rfc2047.encoded_word =
  { charset: charset
  ; encoding: encoding
  ; raw: string * char * string
  ; data: (string, Rresult.R.msg) result }

val is_normalized : t -> bool

val make : encoding:encoding -> string -> (t, [ `Msg of string ]) result
(** [make ~encoding x] returns an {i encoded} word according [encoding] (Quoted
   Printable encoding or Base64 encoding). [x] must be a valid UTF-8 string. {i
   charset} of {i encoded} word will be, by the way, ["UTF-8"].

   NOTE: If you expect to generate an {i encoded} word with something else than
   UTF-8 (like latin1), we decided to not handle this case and just produce
   valid UTF-8 contents in any cases. *)

val make_exn : encoding:encoding -> string -> t

(** Accessors. *)

val encoding : t -> encoding
val charset : t -> charset
val data : t -> (string, Rresult.R.msg) result

(** Pretty-printer. *)

val pp_charset : charset Fmt.t
val pp_encoding : encoding Fmt.t
val pp : t Fmt.t

(** Equal. *)

val equal_charset : charset -> charset -> bool
val equal_encoding : encoding -> encoding -> bool
val equal : t -> t -> bool

val reconstruct : t -> string
(** [reconstruct t] reconstructs [t] as it is in the mail. *)

module Encoder : sig
  val encoded_word : t Encoder.t
end
