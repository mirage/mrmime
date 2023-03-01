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

type t = Emile.group
(** Type of group of addresses. *)

module Phrase : sig
  type elt
  type 'a t = [] : Peano.z t | ( :: ) : elt * 'a t -> 'a Peano.s t

  val o : elt
  val w : string -> elt
  val e : encoding:Encoded_word.encoding -> string -> elt
  val q : Encoded_word.encoding
  val b : Encoded_word.encoding
  val word : string -> (elt, [> `Msg of string ]) result
  val word_exn : string -> elt
  val coerce : 'a Peano.s t -> Emile.phrase
  val make : 'a t -> (Emile.phrase, [> `Msg of string ]) result
  val v : 'a t -> Emile.phrase
  val to_string : Emile.phrase -> string
end

val make : name:Emile.phrase -> Mailbox.t list -> t option
(** [make ~name mailboxes] makes a new group with name [name] of [mailboxes]. *)

val v : name:Emile.phrase -> Mailbox.t list -> t
(** Same as {!make} but raises an exception if list of mailboxes is empty. *)

(** {2 Equal.} *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)

(** {2 Pretty-printer.} *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer of {!t}. *)

(** {2 Decoder of group.} *)

module Decoder : sig
  val group : t Angstrom.t
end

(** {2 Encoder of group.} *)

module Encoder : sig
  val group : t Prettym.t
end
