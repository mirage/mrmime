type field = Rfc5322.trace

type word = Rfc822.word
type local = Rfc822.local
type phrase = Rfc5322.phrase
type literal_domain = Rfc5321.literal_domain =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Ext of string * string
type domain = Rfc5322.domain
type mailbox = Rfc5322.mailbox =
  { name : phrase option
  ; local : local
  ; domain : domain * domain list }
type t

val number : t -> Number.t
val location : t -> Location.t
val length : t -> int
val pp : t Fmt.t

val reduce : (Number.t * ([> field ] as 'a) * Location.t) list -> t list -> (t list * (Number.t * 'a * Location.t) list)

module Encoder : sig
  val trace : t Encoder.t
end
