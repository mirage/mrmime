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

type 'a t =
  | Date : Date.t t
  | Mailboxes : Mailbox.t list t
  | Mailbox : Mailbox.t t
  | Addresses : Address.t list t
  | MessageID : MessageID.t t
  | Unstructured : Unstructured.t t
  | Content : Content_type.t t
  | Encoding : Content_encoding.t t
      (** Type of kind of values according RFC2045/RFC5322. *)

type witness =
  | Witness : 'a t -> witness
      (** Witness type to be able to manipulate {!t}. *)

type field = Field : Field_name.t * 'a t * 'a -> field  (** Type of field. *)

val make : Field_name.t -> 'a t -> 'a -> field
(** [make field_name w v] returns a field. *)

val pp : Format.formatter -> field -> unit
(** Pretty-printer of {!field}. *)

(** {2 Decoder of field.} *)

module Decoder : sig
  val field : ?g:witness Field_name.Map.t -> Field_name.t -> field Angstrom.t
end

(** {2 Encoder of field.} *)

module Encoder : sig
  val field : field Prettym.t
end
