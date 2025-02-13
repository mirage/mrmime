(*
 * Copyright (c) 2025 Romain Calascibetta <romain.calascibetta@gmail.com>
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

(** This module provides an alternative to {!Unstructured} which handles encoded
    words.  It is meant for the Subject and other unstructured header fields,
    where RFC-2047 adds additional semantics on top of the SMTP protocol.  The
    addition of encoded words allows sending UTF-8 content in a 7 bit protocol,
    like SMTP without the SMTPUTF8 extension.

    When decoding, it is necessary to pass the following witness
    {[
      let witness =
	Field_name.Map.singleton Field_name.subject
	  Field.(Witness Unstructured_with_encoded)
    ]}
 *)

type elt = [ Unstructured.elt | `Encoded of string * Emile.raw ]

type t = elt list

val pp_elt : Format.formatter -> elt -> unit
(** Pretty printer of {!elt} mostly useful for debugging. *)

val pp : Format.formatter -> t -> unit
(** Pretty printer of {!t} mostly useful for debugging. *)

module Decoder : sig
  val unstructured_with_encoded : unit -> t Angstrom.t
  (** [unstructured_with_encoded ()] creates a single-use Angstrom parser for
      unstructured content with the encoded words. *)
end

module Encoder : sig
  val unstructured_with_encoded : t Prettym.t
  (** Encodes {!t} as unstructured text with encoded words.  The result is 7
      bits if all non-ASCII characters occur within encoded words. *)
end

module Craft : sig
  val b : Encoded_word.encoding
  val q : Encoded_word.encoding
  (** [q] and [p] are the two possible encodings for {!e}, imported here for
      convenience. *)

  val sp : int -> elt list
  (** [sp n] adds [n] spaces which may be wrapped. *)

  val v : string -> elt list
  (** [v text] will produce [text] literally. *)

  val e : encoding: Encoded_word.encoding -> string -> elt list
  (** [e ~encoding text] will produce [text] encoded as [encoding]. *)

  val compile : elt list list -> t
  (** Assemble a list of the above into a complete field content. *)

  val concat : elt list -> elt list -> elt list
  (** List concatenation, specialized for our purpose. *)

  val (@) : elt list -> elt list -> elt list
  (** This is an alias for {!concat}. *)
end
