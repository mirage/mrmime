type field = Rfc5322.resent
type t

val equal : t -> t -> bool
val number : t -> Number.t option
val length : t -> int
val pp : t Fmt.t

val reduce : (Number.t * ([> field ] as 'a) * Location.t) list -> t list -> (t list * (Number.t * 'a * Location.t) list)

module Encoder : sig
  val resent : t Encoder.t
end
