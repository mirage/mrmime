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

type domain = [ `Literal of string | `Domain of string list ]
type t = Emile.local * domain

module Domain : sig
  (** An RFC 822 domain can be constructed in two ways.
      This construction {b differs} from {!Address.domain}.
      We can construct a common domain via:

      {[
        let isomorphism = Domain.(v domain [ a "isomorphis"; a "me" ]) ;;
        val isomorphism : domain = `Domain ["isomorphis"; "me"]
        Domain.to_string isomorphism ;;
        - : string = "isomorphis.me"
      ]}

      Or a {i literal} domain:

      {[
        let x25519 = Domain.(v literal "x25519") ;;
        val x25519 : domain = `Literal "x25519"
        Domain.to_string x25519 ;;
        - : string = "[x25519]"
      ]} *)

  type atom = [ `Atom of string ]
  type literal = [ `Literal of string ]

  type 'a domain =
    | ( :: ) : atom * 'a domain -> 'a Peano.s domain
    | [] : Peano.z domain

  type 'a t
  (** Kind of domain according RFC 822.

      {ul
      {- An usual {!domain} which is a non-empty list of {!atom} elements}
      {- A [`Literal] domain which is a string surrounded by brackets.}} *)

  val atom : string -> atom option
  (** [atom x] returns a safe {!atom} element. If [x] does not respect RFC 5322,
     it returns [None]. It accepts any characters excepts controls, space and
     specials characters - for instance, brackets are not allowed. *)

  val atom_exn : string -> atom
  (** Same as {!atom} but raises an [Invalid_argument] instead [None]. *)

  val a : string -> atom
  (** Alias of {!atom_exn}. *)

  val literal : string -> literal option
  (** [literal x] returns a {!literal} domain. If [x] does not respect RFC 5321,
     it returns [None]. It will try to escape control characters
     (with {!escape_string}). *)

  val literal_exn : string -> literal
  (** Same as {!literal} but raises an [Invalid_argument] instead to return [None]. *)

  val domain : 'a domain t
  (** Kind of domain. *)

  val default : string t
  (** Kind of {!literal}. *)

  val make :
    'a t -> 'a -> [ `Literal of string | `Domain of string list ] option
  (** [make kind v] returns a safe domain. It can fail if an user-defined
     literal-domain ({!Literal_domain.extension}), a {!literal} domain or a
     {!domain} don't follow standards:

     {ul
     {- for a {!literal}, [make] returns [None] if {!literal} returns [None]}
     {- for a {!domain}, [make] returns [None] if list of {!atom} is empty}} *)

  val v : 'a t -> 'a -> [ `Literal of string | `Domain of string list ]
  (** Same as {!make} but raises an [Invalid_argument] instead [None]. *)

  val to_string : [ `Literal of string | `Domain of string list ] -> string
  (** [to_string x] returns a string which represents [x] as is it in a e-mails. *)
end

(** {2 Pretty-printers.} *)

val pp : Format.formatter -> t -> unit

(** {2 Equals.} *)

val equal : t -> t -> bool

(** {2 Decoder of message ID.} *)

module Decoder : sig
  val message_id : t Angstrom.t
end

val of_string : string -> (t, [> `Msg of string ]) result
(** [of_string x] tries to parse [x] as a Message-ID. *)

(** {2 Encoder of message ID.} *)

module Encoder : sig
  val domain : domain Prettym.t
  val message_id : t Prettym.t
end
