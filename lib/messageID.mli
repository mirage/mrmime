type word = Rfc822.word
type domain = Rfc822.nonsense Rfc822.domain
type local = Rfc822.local
type t = Rfc822.nonsense Rfc822.msg_id

val pp_word : word Fmt.t
val pp_domain : domain Fmt.t
val pp_local : local Fmt.t
val pp : t Fmt.t

val equal_word : word -> word -> bool
val equal_local : local -> local -> bool
val equal_domain : domain -> domain -> bool
val equal : t -> t -> bool

module Encoder : sig
  val domain : domain Encoder.encoding
  val message_id : t Encoder.encoding
end
