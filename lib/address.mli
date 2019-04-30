type t = [`Group of Group.t | `Mailbox of Mailbox.t]
(** Type of address, an address may either be an individual mailbox, or a group
   of mailboxes. *)

val group : Group.t -> t
(** [group g] returns an address from a {!Group.t}. *)

val mailbox : Mailbox.t -> t
(** [mailbox m] returns an address from a {!Mailbox.t}. *)

(** {2 Pretty-printers.} *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

(** {2 Encoder of address.} *)

module Encoder : sig
  val address : t Encoder.encoding
  val addresses : t list Encoder.encoding
end
