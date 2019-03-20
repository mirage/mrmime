module type VALUE = sig
  type t

  val pp : t Fmt.t
  val equal : t -> t -> bool
end
