type phrase = Rfc5322.phrase
type phrase_or_message_id = Rfc5322.phrase_or_message_id

module Field : sig
  type t = string

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val capitalize : t -> t
  val canonicalize : t -> t
  val pp : t Fmt.t
  val of_string : string -> (t, [ `Msg of string ]) result
end

module Map : Map.S with type key = Field.t
module Set : Set.S with type elt = Number.t

val pp_phrase : phrase Fmt.t
val pp_message_id : MessageID.t Fmt.t
val pp_phrase_or_message_id : phrase_or_message_id Fmt.t

type 'a field
type value = V : 'a field -> value
type binding = B : 'a field * 'a * Location.t -> binding

val date : Date.t field
val from : Mailbox.t list field
val sender : Mailbox.t field
val reply_to : Address.t list field
val too : Address.t list field
val cc : Address.t list field
val bcc : Address.t list field
val subject : Unstructured.t field
val message_id : MessageID.t field
val in_reply_to : phrase_or_message_id list field
val references : phrase_or_message_id list field
val comments : Unstructured.t field
val keywords : phrase list field
val resent : Resent.t field
val trace : Trace.t field
val field : Field.t -> Unstructured.t field
val unsafe : Field.t -> Unstructured.t field
val line : string field

val pp_value_of_field : 'a field -> 'a Fmt.t
val field_to_string : 'a field -> string
val field_of_string : string -> (value, Rresult.R.msg) result

module Value : sig
  type t =
    | Date of Date.t
    | From of Mailbox.t list
    | Sender of Mailbox.t
    | ReplyTo of Address.t list
    | To of Address.t list
    | Cc of Address.t list
    | Bcc of Address.t list
    | Subject of Unstructured.t
    | MessageID of MessageID.t
    | InReplyTo of phrase_or_message_id list
    | References of phrase_or_message_id list
    | Comments of Unstructured.t
    | Keywords of phrase list
    | Resent of Resent.t
    | Trace of Trace.t
    | Field of string * Unstructured.t
    | Unsafe of string * Unstructured.t
    | Line of string

  val pp : t Fmt.t
  val of_field : 'a field -> 'a -> t
end

type t

val default : t
val pp : t Fmt.t
val get : 'a field -> t -> ('a * Location.t) list
val get_fields : t -> ((Field.t * Unstructured.t) * Location.t) list
val get_unsafes : t -> ((Field.t * Unstructured.t) * Location.t) list

val with_date : ?location:Location.t -> Number.t -> t -> Date.t -> t
val with_from : ?location:Location.t -> Number.t -> t -> Mailbox.t list -> t
val with_sender : ?location:Location.t -> Number.t -> t -> Mailbox.t -> t
val with_reply_to : ?location:Location.t -> Number.t -> t -> Address.t list -> t
val with_to : ?location:Location.t -> Number.t -> t -> Address.t list -> t
val with_cc : ?location:Location.t -> Number.t -> t -> Address.t list -> t
val with_bcc : ?location:Location.t -> Number.t -> t -> Address.t list -> t
val with_subject : ?location:Location.t -> Number.t -> t -> Unstructured.t -> t
val with_message_id : ?location:Location.t -> Number.t -> t -> MessageID.t -> t
val with_in_reply_to : ?location:Location.t -> Number.t -> t -> phrase_or_message_id list -> t
val with_references : ?location:Location.t -> Number.t -> t -> phrase_or_message_id list -> t
val with_comments : ?location:Location.t -> Number.t -> t -> Unstructured.t -> t
val with_keywords : ?location:Location.t -> Number.t -> t -> phrase list -> t
val with_field : ?location:Location.t -> Number.t -> t -> Field.t -> Unstructured.t -> t

val fold : Number.t -> (([> Rfc5322.field ] as 'a) * Location.t) list -> t -> (t * (Number.t * 'a * Location.t) list)
