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

type t = [ `Group of Group.t | `Mailbox of Mailbox.t ]
(** Type of address, an address may either be an individual mailbox, or a group
   of mailboxes. *)

val group : Group.t -> t
(** [group g] returns an address from a {!Group.t}. *)

val mailbox : Mailbox.t -> t
(** [mailbox m] returns an address from a {!Mailbox.t}. *)

(** {2 Equals.} *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)

(** {2 Pretty-printers.} *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer of {!t}. *)

(** {2 Decoder of address.} *)

module Decoder : sig
  val address : t Angstrom.t
  val address_list : t list Angstrom.t
end

(** {2 Encoder of address.} *)

module Encoder : sig
  val address : t Prettym.t
  val addresses : t list Prettym.t
end
