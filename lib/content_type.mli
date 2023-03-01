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

(** Content-type module. *)

val is_qtext : char -> bool
val is_token : char -> bool

module Type : sig
  type discrete = [ `Text | `Image | `Audio | `Video | `Application ]
  type composite = [ `Message | `Multipart ]
  type extension = [ `Ietf_token of string | `X_token of string ]

  type t =
    [ `Text
    | `Image
    | `Audio
    | `Video
    | `Application
    | `Message
    | `Multipart
    | `Ietf_token of string
    | `X_token of string ]
  (** Type of ... type. *)

  val text : t
  (** Text type. *)

  val image : t
  (** Image type. *)

  val audio : t
  (** Audio type. *)

  val video : t
  (** Video type. *)

  val application : t
  (** Application type. *)

  val message : t
  (** Message type. *)

  val multipart : t
  (** Multipart type. *)

  val ietf : string -> (t, [> `Msg of string ]) result
  (** Type defined by IETF. *)

  val extension : string -> (t, [> `Msg of string ]) result
  (** User-defined type. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer of {!t}. *)

  val compare : t -> t -> int
  (** Comparison of {!t}. *)

  val equal : t -> t -> bool
  (** Equal of {!t}. *)

  val default : t
  (** Default value of type according to RFC 2045. *)

  val is_discrete : t -> bool
  val is_multipart : t -> bool
  val is_message : t -> bool
  val to_string : t -> string
  val of_string : string -> (t, [> `Msg of string ]) result
end

module Subtype : sig
  type t = [ `Ietf_token of string | `Iana_token of string | `X_token of string ]
  (** Type of sub-type. *)

  val ietf : string -> (t, [> `Msg of string ]) result
  (** Sub-type defined by IETF. *)

  val iana : Type.t -> string -> (t, [> `Msg of string ]) result
  (** Sub-type from IANA database. Returns [Error] if sub-type
      is not a part of the IANA database. *)

  val iana_exn : Type.t -> string -> t
  val v : Type.t -> string -> t

  val extension : string -> (t, [> `Msg of string ]) result
  (** User-defined sub-type. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer of {!t}. *)

  val compare : t -> t -> int
  (** Comparison on {!t}. *)

  val equal : t -> t -> bool
  (** Equal on {!t}. *)

  val default : t
  (** Default value of sub-type acccording to RFC 2045. *)

  val to_string : t -> string
end

module Parameters : sig
  module Map : module type of Map.Make (String)

  type key = string
  (** Type of parameter key. *)

  type value = [ `String of string | `Token of string ]
  (** Type of parameter value. *)

  type t = value Map.t
  (** Type of parameters. *)

  val of_list : (key * value) list -> t
  (** Make {!t} from an association list. *)

  val key : string -> (key, [> `Msg of string ]) result
  (** [key v] makes a new key (according to RFC 2045 - otherwise, it returns an
     error). *)

  val key_exn : string -> key
  val k : string -> key

  val value : string -> (value, [> `Msg of string ]) result
  (** [value v] makes a new value (according to RFC 2045 - otherwise, it returns
      an error). *)

  val value_exn : string -> value
  val v : string -> value

  val empty : t
  (** Empty parameters. *)

  val mem : key -> t -> bool
  (** [mem key t] returns true if [key] exists in [t]. Otherwise, it retunrs false. *)

  val add : key -> value -> t -> t
  (** [add key value t] adds [key] binded with [value] in [t]. *)

  val singleton : key -> value -> t
  (** [singleton key value] makes a new {!t} with [key] binded with [value]. *)

  val remove : key -> t -> t
  (** [delete key t] deletes [key] and binded value from [t]. *)

  val find : key -> t -> value option
  (** [find key t] returns value binded with [key] in [t]. *)

  val iter : (key -> value -> unit) -> t -> unit
  (** [iter f t] applies [f] on any bindings availables in [t]. *)

  val pp_key : Format.formatter -> key -> unit
  (** Pretty-printer of {!key}. *)

  val pp_value : Format.formatter -> value -> unit
  (** Pretty-printer of {!value}. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printers of {!t}. *)

  val compare : t -> t -> int
  (** Comparison on {!t}. *)

  val equal : t -> t -> bool
  (** Equal on {!t}. *)

  val default : t
  (** Same as {!empty}. *)

  val to_list : t -> (key * value) list
end

type t =
  { ty : Type.t;
    subty : Subtype.t;
    parameters : (string * Parameters.value) list
  }
(** Type of Content-Type value. *)

val default : t
(** Default Content-Type value according to RFC 2045. *)

val make : Type.t -> Subtype.t -> Parameters.t -> t
(** [make ty subty params] makes a new Content-Type value. *)

val ty : t -> Type.t
(** Return type of Content-Type value. *)

val subty : t -> Subtype.t
(** Return sub-type of Content-Type value. *)

val parameters : t -> (Parameters.key * Parameters.value) list
(** Returns parameters of Content-Type value. *)

val is_discrete : t -> bool
val is_multipart : t -> bool
val is_message : t -> bool
val with_type : t -> Type.t -> t
val with_subtype : t -> Subtype.t -> t
val with_parameter : t -> Parameters.key * Parameters.value -> t
val boundary : t -> string option

(** {2 Pretty-printers.} *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer of {!t}. *)

(** {2 Equals.} *)

val equal : t -> t -> bool
(** Equal of {!t}. *)

(** {2 Decoder of content-type.} *)

module Decoder : sig
  val ty : Type.t Angstrom.t
  val subty : Type.t -> Subtype.t Angstrom.t
  val content : t Angstrom.t
end

(** {2 Encoder of content-type.} *)

module Encoder : sig
  val ty : Type.t Prettym.t
  val subty : Subtype.t Prettym.t
  val content_type : t Prettym.t
end
