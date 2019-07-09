type phrase = Rfc5322.phrase
type phrase_or_message_id = Rfc5322.phrase_or_message_id

type 'a t =
  | Date : Date.t t
  | From : Mailbox.t list t
  | Sender : Mailbox.t t
  | ReplyTo : Address.t list t
  | To : Address.t list t
  | Cc : Address.t list t
  | Bcc : Address.t list t
  | Subject : Unstructured.t t
  | MessageID : MessageID.t t
  | InReplyTo : phrase_or_message_id list t
  | References : phrase_or_message_id list t
  | Comments : Unstructured.t t
  | Keywords : phrase list t
  | Field : Field_name.t -> Unstructured.t t
  | Content : Content.t t
  | Resent : Resent.t t
  | Trace : Trace.t t

type 'a v =
  | Date : Date.t v
  | Mailboxes : Mailbox.t list v
  | Mailbox : Mailbox.t v
  | Addresses : Address.t list v
  | MessageID : Rfc822.nonsense Rfc822.msg_id v
  | Phrase_or_message_id : phrase_or_message_id list v
  | Unstructured : Unstructured.t v
  | Phrases : phrase list v
  | Content : Content.t v
  | Resent : Resent.t v
  | Trace : Trace.t v

type field_name = Field_name : 'a t -> field_name
type field_value = Field_value : 'a v -> field_value
type field = Field : 'a t * 'a -> field

val equal : 'a t -> 'b t -> ('a, 'b) Refl.t option
val make : 'a t -> 'a -> field
val ( $ ) : 'a t -> 'a -> field

val of_field_name : Field_name.t -> field_name
val to_field_name : 'a t -> Field_name.t

val field_name : field -> Field_name.t
val field_value : 'a t -> 'a v

val pp_of_field_value : 'a v -> 'a Fmt.t
val pp_of_field_name : 'a t -> 'a Fmt.t

val of_rfc5322_field : Rfc5322.field -> field

module Encoder : sig
  val field : field Encoder.t
end
