type phrase = Rfc5322.phrase
type phrase_or_message_id = Rfc5322.phrase_or_message_id

val pp_phrase : phrase Fmt.t
val pp_phrase_or_message_id : phrase_or_message_id Fmt.t

module Value : sig
  type t =
    | Field : Unstructured.t -> t
    | Unsafe : Unstructured.t -> t
    | Resent : Resent.Value.t list -> t
    | Trace : Trace.Value.t list -> t
    | Date : Date.t -> t
    | From : Mailbox.t list -> t
    | Sender : Mailbox.t -> t
    | ReplyTo : Address.t list -> t
    | To : Address.t list -> t
    | Cc : Address.t list -> t
    | Bcc : Address.t list -> t
    | Subject : Unstructured.t -> t
    | MessageID : MessageID.t -> t
    | InReplyTo : phrase_or_message_id list -> t
    | References : phrase_or_message_id list -> t
    | Comments : Unstructured.t -> t
    | Keywords : phrase list -> t

  val pp : t Fmt.t
end

type t

val empty : t
val pp : t Fmt.t
val get : Field.t -> t -> (Value.t * Location.t) list
val get_first : Field.t -> t -> Value.t * Location.t
val fold : Number.t -> (([> Rfc5322.field ] as 'a) * Location.t) list -> t -> (t * (Number.t * 'a * Location.t) list)
