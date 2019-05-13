type mail = [ Rfc2045.field | Rfc2045.field_version ]
type part = Rfc2045.field
type t

val default : t
val make : ?encoding:Content_encoding.t -> ?id:MessageID.t -> Content_type.t -> t
val number : t -> Number.t option
val length : t -> int
val empty : t

val add : Content_field.field -> t -> t
val ( & ) : Content_field.field -> t -> t

val ty : t -> Content_type.Type.t
val subty : t -> Content_type.Subtype.t
val encoding : t -> Content_encoding.t
val parameters : t -> Content_type.Parameters.t

val pp : t Fmt.t

val reduce_as_part : ((Number.t * ([> part] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list)
val reduce_as_mail : ((Number.t * ([> mail] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list)

module Encoder : sig
  val content : t Encoder.t
end

