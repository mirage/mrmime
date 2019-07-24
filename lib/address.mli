type t = [`Group of Group.t | `Mailbox of Mailbox.t]
(** Type of address, an address may either be an individual mailbox, or a group
   of mailboxes. *)

val group : Group.t -> t
(** [group g] returns an address from a {!Group.t}. *)

val mailbox : Mailbox.t -> t
(** [mailbox m] returns an address from a {!Mailbox.t}. *)

(** {2 Equals.} *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)

(** {2 Pretty-printers.} *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

(** {2 Encoder of address.} *)

module Encoder : sig
  val address : t Encoder.t
  val addresses : t list Encoder.t
end

(** {2 Unstructured cast.} *)

val addresses_to_unstructured : field_name:Field_name.t -> t list -> Unstructured.t
(** [addresses_to_unstructured ~field_name addresses] map a list of addresses to
   an {!Unstructured.t} value. Should never fail - in this case, it raises a
   [Failure]. *)

val to_unstructured : field_name:Field_name.t -> t -> Unstructured.t
(** [to_unstructured ~field_name t] map an {i address} (eg. {!t}) to an
   {!Unstructured.t} value. Should never fail - in this case, it raises a
   [Failure]. *)
