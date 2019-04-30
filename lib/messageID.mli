type word = Rfc822.word
type domain = Rfc822.nonsense Rfc822.domain
type local = Rfc822.local
type t = Rfc822.nonsense Rfc822.msg_id

(** {2 Pretty-printers.} *)

val pp_word : word Fmt.t
val pp_domain : domain Fmt.t
val pp_local : local Fmt.t
val pp : t Fmt.t

(** {2 Equals.} *)

val equal_word : word -> word -> bool
val equal_local : local -> local -> bool
val equal_domain : domain -> domain -> bool
val equal : t -> t -> bool

(** {2 Encoder of message ID.} *)

module Encoder : sig
  val domain : domain Encoder.encoding
  val message_id : t Encoder.encoding
end

val to_unstructured : field_name:Field_name.t -> t -> Unstructured.t
(** [to_unstructured ~field_name t] casts [t] to an {!Unstructured.t} value -
   which is more general. Returned value respects limit of 1000 characters per
   lines and [to_unstructured] puts [FWS] token ([\r\n] plus, at least, one
   space) to fit under this limit. *)
