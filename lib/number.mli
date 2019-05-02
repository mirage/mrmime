(** Number is an positive integer. *)

type t = private int
(** Type of number. *)

val zero : t
(** [zero] is [0]. *)

val succ : t -> t
(** [succ t] returns [t + 1]. *)

val pred : t -> t option
(** [pred t] returns [t - 1] only if results is upper or equal to [0]. *)

val compare : t -> t -> int
(** Comparison on {!t}. *)

val equal : t -> t -> bool
(** Equal on {!t}. *)

val of_int_exn : int -> t
(** [of_int_exn n] makes a new number. It raises an exception if [n < 0]. *)

val max : t -> t -> t
val min : t -> t -> t
val add : t -> t -> t
