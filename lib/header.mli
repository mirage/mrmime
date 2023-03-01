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

type t
(** Type of the header. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer of {!pp}. *)

val assoc : Field_name.t -> t -> Field.field list
(** [assoc field_name header] returns all values associated to [field_name]
   (several can exist into a [header]). It returns order, value and location if
   it comes from a source. *)

val remove_assoc : Field_name.t -> t -> t
(** [remove_assoc field_name t] removes all values associated to [field_name]. *)

val concat : t -> t -> t
(** [concat t0 t1] concats [t0] with [t1] - any field-name existing in both
   are not deleted/replaced. *)

val exists : Field_name.t -> t -> bool
(** [exists field_name t] is [true] if [field_name] exists in [t]. *)

val add : Field_name.t -> 'a Field.t * 'a -> t -> t
(** [add field_name (w, v) t] adds a new field-name with value v. [add]
   does not replace [field_name] if it already exists into [t]. *)

val add_unless_exists : Field_name.t -> 'a Field.t * 'a -> t -> t
(** [add_unless_exists field_name (w, v) t] is a collection of header fields that
   is the same as [t] if [t] already includes [field_name], and otherwise is
   equivalent to [add field_name (w, v) t]. *)

val replace : Field_name.t -> 'a Field.t * 'a -> t -> t
(** [replace field_name (w, v) t] replaces existing field-name [field_name] in [t]
   by the new value [v]. If [field_name] does not exist, it adds it. *)

val of_list : Field.field list -> t
(** [of_list l] returns a header from the list [l] (without location). *)

val of_list_with_location : Field.field Location.with_location list -> t
val to_list_with_location : t -> Field.field Location.with_location list
val to_list : t -> Field.field list

val empty : t
(** [empty] is an empty header which does not have any default values. *)

val content_type : t -> Content_type.t
(** [content_type header] returns {!Content_type.t} of the given header. If it does not
   exist, it returns {!Content_type.default}. *)

val content_encoding : t -> Content_encoding.t
(** [content_encoding header] returns {!Content_encoding.t} of the given header. If it
   does not exist, it returns {!Content_encoding.default}. *)

val message_id : t -> MessageID.t option
(** [message_id header] returns a {!MessageID.t} if it exists. Otherwise it returns
   [None]. *)

val length : t -> int
(** [length hdr] returns the length of the given header [hdr]. *)

(** {2 Decoder of header.} *)

module Decoder : sig
  val header : Field.witness Field_name.Map.t option -> t Angstrom.t
end

(** {2 Encoder of header.} *)

module Encoder : sig
  val header : t Prettym.t
end

val to_stream : t -> unit -> string option
(** [to_stream header] returns a stream of the given header which can be used
   into protocol like SMTP. *)
