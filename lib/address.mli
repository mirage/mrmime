type t = Rfc5322.address

val group : Group.t -> t
val mailbox : Mailbox.t -> t

val pp : t Fmt.t
