type t = private string

val compare : t -> t -> int
val equal : t -> t -> bool
val capitalize : t -> t
val canonicalize : t -> t
val pp : t Fmt.t
val of_string : string -> (t, [ `Msg of string ]) result
val of_string_exn : string -> t
val v : string -> t
val prefixed_by : string -> t -> bool

module Encoder : sig
  val field : t Encoder.encoding
end
