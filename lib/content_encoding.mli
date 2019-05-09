type t = Rfc2045.mechanism
(** Type for standard mechanism for encoding. *)

val pp : t Fmt.t
(** Pretty-printer for {!t}. *)

val default : t
(** An encoding type of [7bit] requires that the body is already in a 7bit
   mail-ready representation. This is the default value. *)

module Encoder : sig
  val mechanism : t Encoder.t
end
