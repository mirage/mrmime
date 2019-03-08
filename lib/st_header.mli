module Refl : sig type ('a, 'b) t = Refl : ('a, 'a) t end

module Value : sig
  type phrase_or_message_id = [`Phrase of Rfc5322.phrase | `MessageID of Rfc822.nonsense Rfc822.msg_id]
  type trace = Rfc5322.mailbox option * ([ `Addr of Rfc5322.mailbox | `Domain of Rfc5322.domain | `Word of Rfc822.word ] list * Rfc5322.date option) list

  type 'a t =
    | Date : Rfc5322.date t
    | Mailboxes : Rfc5322.mailbox list t
    | Mailbox : Rfc5322.mailbox t
    | Addresses : Rfc5322.address list t
    | MessageID : Rfc822.nonsense Rfc822.msg_id t
    | Phrase_or_message_id : phrase_or_message_id list t
    | Unstructured : Rfc5322.unstructured t
    | Phrases : Rfc5322.phrase list t
    | Trace : trace t
    | Lines : string list t

  type binding =
    | B : Field.t * 'a t * 'a * Location.t -> binding
    | L : string list * Location.t -> binding
  type value = V : 'a t -> value

  val of_string : string -> (value, Rresult.R.msg) result
  val pp : 'a t Fmt.t
  val pp_of_value : 'a t -> 'a Fmt.t

  val equal : 'a t -> 'b t -> ('a, 'b) Refl.t option
end

type 'value decoder

type 'value decode =
  [ `Field of 'value
  | `Other of Field.t * string
  | `Lines of string list
  | `Await
  | `End
  | `Malformed of string ]

val decoder : field:Field.t -> 'value Value.t -> Bigstringaf.t -> 'value decoder
val decode : 'value decoder -> 'value decode
val src : 'value decoder -> string -> int -> int -> (unit, Rresult.R.msg) result
