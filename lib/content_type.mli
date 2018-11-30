module Type : sig
  type t

  val text : t
  val image : t
  val audio : t
  val video : t
  val application : t
  val message : t
  val multipart : t
  val ietf : string -> t option
  val extension : string -> t option

  (* / *)

  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Subtype : sig
  type t

  val ietf : string -> t option
  val iana : Type.t -> string -> t option
  val extension : string -> t option

  (* / *)

  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Parameters : sig
  type t
  type key
  type value

  val key : string -> key option
  val value : string -> value option

  (* / *)

  val empty : t
  val mem : key -> t -> bool
  val add : key -> value -> t -> t
  val singleton : key -> value -> t
  val remove : key -> t -> t
  val find : key -> t -> value option
  val iter : (key -> value -> unit) -> t -> unit

  (* / *)

  val pp_key : key Fmt.t
  val pp_value : value Fmt.t
  val pp : t Fmt.t

  (* / *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type t = Rfc2045.content

val make : Type.t -> Subtype.t -> Parameters.t -> t

(* / *)

val pp : t Fmt.t
val equal : t -> t -> bool
