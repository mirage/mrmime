type word = Rfc822.word
type domain = Rfc822.nonsense Rfc822.domain
type local = Rfc822.local
type t = Rfc822.nonsense Rfc822.msg_id

module Domain : sig
  (** An RFC 822 domain can be constructed in two ways.
      This construction {b differs} from {!Address.domain}.
      We can construct a common domain via:

      {[
        let isomorphism = Domain.(v domain [ a "isomorphis"; a "me" ]) ;;
        val isomorphism : domain = `Domain ["isomorphis"; "me"]
        Domain.to_string isomorphism ;;
        - : string = "isomorphis.me"
      ]}

      Or a {i literal} domain:

      {[
        let x25519 = Domain.(v literal "x25519") ;;
        val x25519 : domain = `Literal "x25519"
        Domain.to_string x25519 ;;
        - : string = "[x25519]"
      ]} *)

  type atom = [`Atom of string]
  type literal = [`Literal of string]

  type 'a domain =
    | ( :: ) : atom * 'a domain -> 'a Peano.s domain
    | [] : Peano.z domain

  type 'a t
  (** Kind of domain according RFC 822.

      {ul
      {- An usual {!domain} which is a non-empty list of {!atom} elements}
      {- A [`Literal] domain which is a string surrounded by brackets.}} *)

  val atom : string -> atom option
  (** [atom x] returns a safe {!atom} element. If [x] does not respect RFC 5322,
     it returns [None]. It accepts any characters excepts controls, space and
     specials characters - for instance, brackets are not allowed. *)

  val atom_exn : string -> atom
  (** Same as {!atom} but raises an [Invalid_argument] instead [None]. *)

  val a : string -> atom
  (** Alias of {!atom_exn}. *)

  val literal : string -> literal option
  (** [literal x] returns a {!literal} domain. If [x] does not respect RFC 5321,
     it returns [None]. It will try to escape control characters
     (with {!escape_string}). *)

  val literal_exn : string -> literal
  (** Same as {!literal} but raises an [Invalid_argument] instead to return [None]. *)

  val domain : 'a domain t
  (** Kind of domain. *)

  val default : string t
  (** Kind of {!literal}. *)

  val make : 'a t -> 'a -> Rfc822.nonsense Rfc822.domain option
  (** [make kind v] returns a safe domain. It can fail if an user-defined
     literal-domain ({!Literal_domain.extension}), a {!literal} domain or a
     {!domain} don't follow standards:

     {ul
     {- for a {!literal}, [make] returns [None] if {!literal} returns [None]}
     {- for a {!domain}, [make] returns [None] if list of {!atom} is empty}} *)

  val v : 'a t -> 'a -> Rfc822.nonsense Rfc822.domain
  (** Same as {!make} but raises an [Invalid_argument] instead [None]. *)

  val to_string : Rfc822.nonsense Rfc822.domain -> string
  (** [to_string x] returns a string which represents [x] as is it in a e-mails. *)
end

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
  val domain : domain Encoder.t
  val message_id : t Encoder.t
end

val to_unstructured : field_name:Field_name.t -> t -> Unstructured.t
(** [to_unstructured ~field_name t] casts [t] to an {!Unstructured.t} value -
   which is more general. Returned value respects limit of 1000 characters per
   lines and [to_unstructured] puts [FWS] token ([\r\n] plus, at least, one
   space) to fit under this limit. *)
