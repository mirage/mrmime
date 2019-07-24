type t
(** Type of the header. *)

type value = Value : 'a Field.v * 'a -> value
(** Type of a value. *)

val field_to_value : Field.field -> value
(** [field_to_value field] extracts value of [field] with witness type of it. *)

val pp_value : value Fmt.t
(** Pretty-printer of {!value}. *)

val get : Field_name.t -> t -> (Number.t * value * Location.t) list
(** [get field_name header] returns all values associated to [field_name]
   (several can exist into a [header]). It returns order, value and location if
   it comes from a source. *)

val add : Field.field -> t -> t
(** [add field header] adds [field] into [header]. *)

val add_or_replace : Field.field -> t -> t
(** [add_or_replace field header] adds [field] if it does not exist in [header]
   or replace older one by the new one. *)

val empty : t
(** [empty] is an empty header which does not have any default values. *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

val content : t -> Content.t
(** [content header] returns {!Content.t} of the given header. If it does not
   exist, it returns {!Content.default}. *)

val ( & ) : Field.field -> t -> t
(** Alias of {!add}. *)

val reduce : (Number.t * ([> Rfc5322.field ] as 'a) * Location.t) list -> t -> (t * (Number.t * 'a * Location.t) list)

(** {2 Encoder of header.} *)

module Encoder : sig
  val header : t Encoder.t
end

val to_stream : t -> (unit -> string option)
(** [to_stream header] returns a stream of the given header which can be used
   into protocol like SMTP. *)

(** / *)

val to_string : t -> string
