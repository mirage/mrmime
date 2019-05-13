type 'a t =
  | Date : Date.t t
  | From : Mailbox.t list t
  | Sender : Mailbox.t t
  | To : Address.t list t
  | Cc : Address.t list t
  | Bcc : Address.t list t
  | MessageID : MessageID.t t
  | ReplyTo : Address.t list t
  | Field : Field_name.t -> Unstructured.t t

type 'a v =
  | Date : Date.t v
  | Mailboxes : Mailbox.t list v
  | Mailbox : Mailbox.t v
  | Addresses : Address.t list v
  | MessageID : MessageID.t v
  | Unstructured : Unstructured.t v

type field_name = Field_name : 'a t -> field_name
type field_value = Field_value : 'a v -> field_value
type field = Field : 'a t * 'a -> field

let make : type a. a t -> a -> field =
  fun field_name field_value -> Field (field_name, field_value)

let prefixed_by_resent = Field_name.prefixed_by "Resent"

let of_field_name : Field_name.t -> field_name =
  fun field_name -> match String.lowercase_ascii (field_name :> string) with
    | "resent-date" -> Field_name Date
    | "resent-from" -> Field_name From
    | "resent-sender" -> Field_name Sender
    | "resent-to" -> Field_name To
    | "resent-cc" -> Field_name Cc
    | "resent-bcc" -> Field_name Bcc
    | "resent-message-id" -> Field_name MessageID
    | _ ->
      if prefixed_by_resent field_name
      then Field_name (Field field_name)
      else Fmt.invalid_arg "Invalid Resent field-name: %a" Field_name.pp field_name

let to_field_name : type a. a t -> Field_name.t = function
  | Date -> Field_name.resent_date
  | From -> Field_name.resent_from
  | Sender -> Field_name.resent_sender
  | To -> Field_name.resent_to
  | Cc -> Field_name.resent_cc
  | Bcc -> Field_name.resent_bcc
  | MessageID -> Field_name.resent_message_id
  | ReplyTo -> Field_name.resent_reply_to
  | Field field_name -> field_name

let field_name
  : field -> Field_name.t
  = fun (Field (field_name, _)) -> to_field_name field_name

let field_value : type a. a t -> a v = function
  | Date -> Date
  | From -> Mailboxes
  | Sender -> Mailbox
  | To -> Addresses
  | Cc -> Addresses
  | Bcc -> Addresses
  | MessageID -> MessageID
  | ReplyTo -> Addresses
  | Field _ -> Unstructured

let of_rfc5322_field : Rfc5322.resent -> field = function
  | `ResentDate x -> Field (Date, x)
  | `ResentFrom x -> Field (From, x)
  | `ResentSender x -> Field (Sender, x)
  | `ResentTo x -> Field (To, x)
  | `ResentCc x -> Field (Cc, x)
  | `ResentBcc x -> Field (Bcc, x)
  | `ResentMessageID x -> Field (MessageID, x)
  | `ResentReplyTo x -> Field (ReplyTo, x)

let pp_of_field_value : type a. a v -> a Fmt.t = function
  | Date -> Date.pp
  | Mailboxes -> Fmt.Dump.list Mailbox.pp
  | Mailbox -> Mailbox.pp
  | Addresses -> Fmt.Dump.list Address.pp
  | MessageID -> MessageID.pp
  | Unstructured -> Unstructured.pp

let pp_of_field_name : type a. a t -> a Fmt.t = fun x -> pp_of_field_value (field_value x)
