type t = private int

val zero : t
val succ : t -> t
val pred : t -> t option
val compare : t -> t -> int
val equal : t -> t -> bool
val of_int_exn : int -> t
