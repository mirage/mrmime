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
  ; raw : string
  ; data: (string, Rresult.R.msg) result }

val is_normalized : t -> bool
val make : encoding:encoding -> string -> t option
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

module Encoder : sig
  val encoded_word : t Encoder.t
end
