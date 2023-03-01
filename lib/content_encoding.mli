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

(** Content-Transfer-Encoding value *)

type t =
  [ `Bit7
  | `Bit8
  | `Binary
  | `Quoted_printable
  | `Base64
  | `Ietf_token of string
  | `X_token of string ]
(** Type for standard mechanism for encoding. *)

(** {2 Basic encodings.} *)

val bit8 : t
val bit7 : t
val binary : t
val quoted_printable : t
val base64 : t

val default : t
(** An encoding type of [7bit] requires that the body is already in a 7bit
   mail-ready representation. This is the default value. *)

val of_string : string -> (t, [> `Msg of string ]) result
(** [of_string x] is the standard mechanism [x]. If [x] is an invalid mechanism,
   [of_string] returns an error. *)

(** {2 Equals.} *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)

(** {2 Pretty-printers.} *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for {!t}. *)

(** {2 Decoder of Content-Encoding's value.} *)

module Decoder : sig
  val mechanism : t Angstrom.t
end

(** {2 Encoder of Content-Encoding's value.} *)

module Encoder : sig
  val mechanism : t Prettym.t
end
