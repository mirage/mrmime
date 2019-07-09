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

let equal : type a b. a t -> b t -> (a, b) Refl.t option = fun a b -> match a, b with
  | Date, Date -> Some Refl.Refl
  | From, From -> Some Refl.Refl
  | Sender, Sender -> Some Refl.Refl
  | ReplyTo, ReplyTo -> Some Refl.Refl
  | To, To -> Some Refl.Refl
  | Cc, Cc -> Some Refl.Refl
  | Bcc, Bcc -> Some Refl.Refl
  | Subject, Subject -> Some Refl.Refl
  | MessageID, MessageID -> Some Refl.Refl
  | InReplyTo, InReplyTo -> Some Refl.Refl
  | References, References -> Some Refl.Refl
  | Comments, Comments -> Some Refl.Refl
  | Keywords, Keywords -> Some Refl.Refl
  | Field a, Field b -> if Field_name.equal a b then Some Refl.Refl else None
  | Content, Content -> Some Refl.Refl
  | Resent, Resent -> Some Refl.Refl
  | Trace, Trace -> Some Refl.Refl
  | _ -> None

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

let make : type a. a t -> a -> field =
  fun field_name field_value -> Field (field_name, field_value)

let ( $ ) = make

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
  | Content -> Fmt.invalid_arg "to_field_name: Content are folded fields"
  | Resent -> Fmt.invalid_arg "to_field_name: Resent are folded fields"
  | Trace -> Fmt.invalid_arg "to_field_name: Trace are folded field"

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
  | Content -> Content
  | Resent -> Resent
  | Trace -> Trace

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
  | Content -> Content.pp
  | Resent -> Resent.pp
  | Trace -> Trace.pp

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

module Encoder = struct
  include Encoder

  let field_name = Field_name.Encoder.field_name
  let date = Date.Encoder.date
  let mailboxes = Mailbox.Encoder.mailboxes
  let mailbox = Mailbox.Encoder.mailbox
  let addresses = Address.Encoder.addresses
  let unstructured = Unstructured.Encoder.unstructured
  let message_id = MessageID.Encoder.message_id
  let phrase_or_message_id ppf = function
    | `Phrase x -> Mailbox.Encoder.phrase ppf x
    | `MessageID x -> MessageID.Encoder.message_id ppf x
  let phrase = Mailbox.Encoder.phrase
  let phrases =
    let comma = (fun ppf () -> eval ppf [ char $ ','; fws ]), () in
    list ~sep:comma phrase

  let field_and_value f value ppf v =
    eval ppf [ !!field_name; char $ ':'; spaces 1; bbox; !!value; close; new_line ] f v

  let epsilon = (fun t () -> t), ()

  let date = field_and_value Field_name.date date
  let from = field_and_value Field_name.from mailboxes
  let sender = field_and_value Field_name.sender mailbox
  let reply_to = field_and_value Field_name.reply_to addresses
  let _to = field_and_value (Field_name.v "To") addresses
  let cc = field_and_value Field_name.cc addresses
  let bcc = field_and_value Field_name.bcc addresses
  let subject = field_and_value Field_name.subject unstructured
  let message_id = field_and_value Field_name.message_id message_id
  let in_reply_to = field_and_value Field_name.in_reply_to (list ~sep:epsilon phrase_or_message_id)
  let references = field_and_value Field_name.references (list ~sep:epsilon phrase_or_message_id)
  let comments = field_and_value Field_name.comments unstructured
  let keywords = field_and_value Field_name.keywords phrases
  let field field = field_and_value field unstructured
  let unsafe field = field_and_value field unstructured

  let field ppf (Field (k, v)) = match k with
    | Date -> date ppf v
    | From -> from ppf v
    | Sender -> sender ppf v
    | ReplyTo -> reply_to ppf v
    | To -> _to ppf v
    | Cc -> cc ppf v
    | Bcc -> bcc ppf v
    | Subject -> subject ppf v
    | MessageID -> message_id ppf v
    | InReplyTo -> in_reply_to ppf v
    | References -> references ppf v
    | Comments -> comments ppf v
    | Keywords -> keywords ppf v
    | Field field_name -> field field_name ppf v
    | Content -> Content.Encoder.content ppf v
    | Resent -> Resent.Encoder.resent ppf v
    | Trace -> Trace.Encoder.trace ppf v

  let field ppf v = eval ppf [ !!field  ] v
end
