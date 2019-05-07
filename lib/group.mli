type word = Rfc822.word
type phrase = Rfc5322.phrase
type t = Rfc5322.group = {name: phrase; mailboxes: Mailbox.t list}

val make : name:phrase -> Mailbox.t list -> t option
(** [make ~name mailboxes] makes a new group with name [name] of [mailboxes]. *)

val v : name:phrase -> Mailbox.t list -> t
(** Same as {!make} but raises an exception if list of mailboxes is empty. *)

(** {2 Equals.} *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)

(** {2 Pretty-printers.} *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

(** {2 Encoder of group.} *)

module Encoder : sig
  val group : t Encoder.t
end
