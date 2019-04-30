type phrase = Rfc5322.phrase
type phrase_or_message_id = Rfc5322.phrase_or_message_id
type trace =
  Rfc5322.mailbox option
  * ([ `Addr of Rfc5322.mailbox
      | `Domain of Rfc5322.domain
      | `Word of Rfc822.word ] list * Rfc5322.date option) list

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

type 'a v =
  | Date : Date.t v
  | Mailboxes : Mailbox.t list v
  | Mailbox : Mailbox.t v
  | Addresses : Address.t list v
  | MessageID : Rfc822.nonsense Rfc822.msg_id v
  | Phrase_or_message_id : phrase_or_message_id list v
  | Unstructured : Unstructured.t v
  | Phrases : phrase list v
  | Trace : trace v

type field_name = Field_name : 'a t -> field_name
type field_value = Field_value : 'a v -> field_value
type field = Field : 'a t * 'a -> field

let make : type a. a t -> a -> field =
  fun field_name field_value -> Field (field_name, field_value)

let of_field_name : Field_name.t -> field_name =
  (* XXX(dinosaure): not really safe where [Field_name] provides theses values. *)
  fun field_name -> match String.lowercase_ascii (field_name :> string) with
    | "date" -> Field_name Date
    | "from" -> Field_name From
    | "sender" -> Field_name Sender
    | "reply-to" -> Field_name ReplyTo
    | "to" -> Field_name To
    | "cc" -> Field_name Cc
    | "bcc" -> Field_name Bcc
    | "subject" -> Field_name Subject
    | "message-id" -> Field_name MessageID
    | "in-reply-to" -> Field_name InReplyTo
    | "references" -> Field_name References
    | "comments" -> Field_name Comments
    | "keywords" -> Field_name Keywords
    | _ -> Field_name (Field field_name)

let to_field_name : type a. a t -> Field_name.t = function
  | Date -> Field_name.date
  | From -> Field_name.from
  | Sender -> Field_name.sender
  | ReplyTo -> Field_name.reply_to
  | To -> Field_name.v "To"
  | Cc -> Field_name.cc
  | Bcc -> Field_name.bcc
  | Subject -> Field_name.subject
  | MessageID -> Field_name.message_id
  | InReplyTo -> Field_name.in_reply_to
  | References -> Field_name.references
  | Comments -> Field_name.comments
  | Keywords -> Field_name.keywords
  | Field field_name -> field_name

let field_name
  : field -> Field_name.t
  = fun (Field (field_name, _)) -> to_field_name field_name

let field_value : type a. a t -> a v = function
  | Date -> Date
  | From -> Mailboxes
  | Sender -> Mailbox
  | ReplyTo -> Addresses
  | To -> Addresses
  | Cc -> Addresses
  | Bcc -> Addresses
  | Subject -> Unstructured
  | MessageID -> MessageID
  | InReplyTo -> Phrase_or_message_id
  | References -> Phrase_or_message_id
  | Comments -> Unstructured
  | Keywords -> Phrases
  | Field _ -> Unstructured

let pp_phrase = Mailbox.pp_phrase
let pp_message_id = MessageID.pp

let pp_phrase_or_message_id ppf = function
  | `Phrase phrase -> Fmt.pf ppf "(`Phrase %a)" (Fmt.hvbox pp_phrase) phrase
  | `MessageID msg_id -> Fmt.pf ppf "(`MessageID %a)" (Fmt.hvbox pp_message_id) msg_id

let pp_of_field_value : type a. a v -> a Fmt.t = function
  | Date -> Date.pp
  | Mailboxes -> Fmt.(Dump.list Mailbox.pp)
  | Mailbox -> Mailbox.pp
  | Addresses -> Fmt.(Dump.list Address.pp)
  | Unstructured -> Unstructured.pp
  | MessageID -> MessageID.pp
  | Phrase_or_message_id -> Fmt.(Dump.list pp_phrase_or_message_id)
  | Phrases -> Fmt.(Dump.list pp_phrase)
  | Trace -> assert false (* TODO *)

let pp_of_field_name : type a. a t -> a Fmt.t = fun x -> pp_of_field_value (field_value x)
(* XXX(dinosaure): [<.>]? *)

let of_rfc5322_field : Rfc5322.field -> field = function
  | `Date x -> Field (Date, x)
  | `From x -> Field (From, x)
  | `Sender x -> Field (Sender, x)
  | `ReplyTo x -> Field (ReplyTo, x)
  | `To x -> Field (To, x)
  | `Cc x -> Field (Cc, x)
  | `Bcc x -> Field (Bcc, x)
  | `MessageID x -> Field (MessageID, x)
  | `InReplyTo x -> Field (InReplyTo, x)
  | `References x -> Field (References, x)
  | `Subject x -> Field (Subject, x)
  | `Comments x -> Field (Comments, x)
  | `Keywords x -> Field (Keywords, x)
  | `Field (field_name, x) -> Field (Field field_name, x)
