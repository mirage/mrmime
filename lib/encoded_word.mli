(*
 * Copyright (c) 2018-2019 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type charset =
  [ `UTF_8
  | `UTF_16
  | `UTF_16BE
  | `UTF_16LE
  | Rosetta.encoding
  | `US_ASCII
  | `Charset of string ]

type encoding = Quoted_printable | Base64

val b : encoding
(** Base64 encoding. *)

val q : encoding
(** Inline quoted-printable encoding. *)

type t =
  { charset : charset;
    encoding : encoding;
    data : (string, [ `Msg of string ]) result
  }

val is_normalized : t -> bool

val make : encoding:encoding -> string -> (t, [ `Msg of string ]) result
(** [make ~encoding x] returns an {i encoded} word according [encoding] (Quoted
   Printable encoding or Base64 encoding). [x] must be a valid UTF-8 string. {i
   charset} of {i encoded} word will be, by the way, ["UTF-8"].

   NOTE: If you expect to generate an {i encoded} word with something else than
   UTF-8 (like latin1), we decided to not handle this case and just produce
   valid UTF-8 contents in any cases. *)

val make_exn : encoding:encoding -> string -> t
(** Alias of {!make} but raises an [Invalid_argument] if it fails. *)

(** Accessors. *)

val encoding : t -> encoding
val charset : t -> charset
val data : t -> (string, [ `Msg of string ]) result

(** Pretty-printer. *)

val pp_charset : Format.formatter -> charset -> unit
val pp_encoding : Format.formatter -> encoding -> unit
val pp : Format.formatter -> t -> unit

(** Equal. *)

val equal_charset : charset -> charset -> bool
val equal_encoding : encoding -> encoding -> bool
val equal : t -> t -> bool

val charset_of_string : string -> charset
(** [charset_of_string s] returns {i charset} of well-formed charset identifier
   [s] (according IANA). *)

val charset_to_string : charset -> string

val normalize_to_utf8 :
  charset:charset -> string -> (string, [ `Msg of string ]) result
(** [normalize_to_utf8 ~charset s] maps a source [s] which is encoded with the
   charset {!charset} and try to map/normalize it to UTF-8. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string v] tries to parse [v] as an encoded-word (according RFC 2047). *)

(** {2 Decoders.} *)

module Decoder : sig
  val encoded_word : t Angstrom.t
end

(** {2 Encoders.} *)

module Encoder : sig
  val encoded_word : t Prettym.t
end
