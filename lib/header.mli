type t
type value = Value : 'a Field.v * 'a -> value

val field_to_value : Field.field -> value
val pp_value : value Fmt.t

val get : Field_name.t -> t -> (Number.t * value * Location.t) list
val add : Field.field -> t -> t
val empty : t
val pp : t Fmt.t
val content : t -> Content.t

val ( & ) : Field.field -> t -> t

val reduce : (Number.t * ([> Rfc5322.field ] as 'a) * Location.t) list -> t -> (t * (Number.t * 'a * Location.t) list)

module Encoder : sig
  val header : t Encoder.t
end

val to_string : t -> string
val to_stream : t -> (unit -> string option)
