type t = Rfc2045.mechanism
(** Type for standard mechanism for encoding. *)

(** {2 Basic encodings.} *)

val bit8 : t
val bit7 : t
val binary : t
val quoted_printable : t
val base64 : t

val default : t
(** An encoding type of [7bit] requires that the body is already in a 7bit
   mail-ready representation. This is the default value. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string x] is the standard mechanism [x]. If [x] is an invalid mechanism,
   [of_string] returns an error. *)

(** {2 Equals.} *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)

(** {2 Pretty-printers.} *)

val pp : t Fmt.t
(** Pretty-printer for {!t}. *)

(** {2 Encoder of Content-Encoding's value.} *)

module Encoder : sig
  val mechanism : t Encoder.t
end
