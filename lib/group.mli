type word = Rfc822.word
type phrase = Rfc5322.phrase
type t = Rfc5322.group = {name: phrase; mailboxes: Mailbox.t list}

val make : name:phrase -> Mailbox.t list -> t option
(** [make ~name mailboxes] makes a new group with name [name] of [mailboxes]. *)

val v : name:phrase -> Mailbox.t list -> t
(** Same as {!make} but raises an exception if list of mailboxes is empty. *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)
