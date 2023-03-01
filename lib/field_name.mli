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

type t = private string
(** Type of field name. *)

val compare : t -> t -> int
(** Comparison function on {!t}. *)

val equal : t -> t -> bool
(** Equal function on {!t}. *)

val capitalize : t -> t
(** [capitalize t] capitalizes [t] - any letter in front of ['-'] will be a
   capital letter. *)

val canonicalize : t -> t
(** [canonicalize t] apply [String.lowercase_ascii] on [t]. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer of {!t}. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] tries to return a field-name. [s] must
   respect standards. Otherwise, we return an [Error]. *)

val of_string_exn : string -> t
(** Same as {!of_string} but raises an {!Invalid_argument} instead of returning
   [Error]. *)

val v : string -> t
(** Alias of {!of_string_exn}. *)

val prefixed_by : string -> t -> bool
(** [of_prefixed_by prefix t] returns [true] if [t] is prefixed by [s]:

    {[
      prefixed_by "Resent" resent_date ;;
      - : bool = true
    ]} *)

(** {2 Decoder of field name.} *)

module Decoder : sig
  val field_name : t Angstrom.t
end

(** {2 Encoder of field name.} *)

module Encoder : sig
  val field_name : t Prettym.t
end

(** {2 Helpers.} *)

(** {3 RFC 5322 Field names.} *)

val date : t
val from : t
val sender : t
val reply_to : t
val cc : t
val bcc : t
val subject : t
val message_id : t
val in_reply_to : t
val references : t
val comments : t
val keywords : t
val received : t
val return_path : t

(** {3 RFC 2045 Field names.} *)

val content_type : t
val content_encoding : t
val mime_version : t
val content_id : t
val content_description : t

(** {3 {i Resent} Field names.} *)

val resent_date : t
val resent_from : t
val resent_sender : t
val resent_to : t
val resent_cc : t
val resent_bcc : t
val resent_message_id : t
val resent_reply_to : t

module Map : Map.S with type key = t
