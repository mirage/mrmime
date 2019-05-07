module O = Map.Make(Number)
module N = Map.Make(struct type t = Number.t let compare a b = Number.compare b a end)

type field = Rfc5322.resent
type t = (Resent_field.field * Location.t) O.t O.t

let ( <.> ) f g = fun x -> f (g x)

let reduce
  : (Number.t * ([> field ] as 'a) * Location.t) list -> t -> (t * (Number.t * 'a * Location.t) list)
  = fun fields resents ->
    let resents = (N.of_seq <.> O.to_seq) resents in
    List.fold_left
      (fun (resents, rest) (n, field, loc) ->
         match field with
         | (`ResentFrom _ | `ResentDate _) as field ->
           let v = Resent_field.of_rfc5322_field field in
           N.add n (O.singleton n (v, loc)) resents, rest
         | #field as field ->
           let v = Resent_field.of_rfc5322_field field in
           (match N.choose_opt resents with
            | Some (i, last) ->
              N.add i (O.add n (v, loc) last) resents
            | None ->
              N.add n (O.singleton n (v, loc)) resents), rest
         | _ -> resents, (n, field, loc) :: rest)
      (resents, []) fields
    |> fun (resents, rest) -> ((O.of_seq <.> N.to_seq) resents, List.rev rest)

let empty = O.empty

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let field = Field_name.Encoder.field
  let date = Date.Encoder.date
  let mailbox = Mailbox.Encoder.mailbox
  let mailboxes = Mailbox.Encoder.mailboxes
  let addresses = Address.Encoder.addresses
  let message_id = MessageID.Encoder.message_id
  let unstructured = Unstructured.Encoder.unstructured

  let field_and_value field_value value_encoding ppf value =
    eval ppf [ !!field; char $ ':'; spaces 1; bbox; !!value_encoding; close; new_line ] field_value value

  let resent_date = field_and_value Field_name.resent_date date
  let resent_from = field_and_value Field_name.resent_from mailboxes
  let resent_sender = field_and_value Field_name.resent_sender mailbox
  let resent_to = field_and_value Field_name.resent_to addresses
  let resent_cc = field_and_value Field_name.resent_cc addresses
  let resent_bcc = field_and_value Field_name.resent_bcc addresses
  let resent_message_id = field_and_value Field_name.resent_message_id message_id
  let resent_reply_to = field_and_value Field_name.resent_reply_to addresses
  let resent_field field = field_and_value field unstructured
  let resent_unsafe field = field_and_value field unstructured

  let resent ppf (_, Resent_field.Field (field_name, v)) = match field_name with
    | Resent_field.Date -> resent_date ppf v
    | Resent_field.From -> resent_from ppf v
    | Resent_field.Sender -> resent_sender ppf v
    | Resent_field.To -> resent_to ppf v
    | Resent_field.Cc -> resent_cc ppf v
    | Resent_field.Bcc -> resent_bcc ppf v
    | Resent_field.MessageID -> resent_message_id ppf v
    | Resent_field.ReplyTo -> resent_reply_to ppf v
    | Resent_field.Field field_name -> resent_field field_name ppf v

  let epsilon = (fun t () -> t), ()

  let resent ppf (_, x) = (list ~sep:epsilon resent) ppf (O.bindings x)
  let resents ppf x = (list ~sep:epsilon resent) ppf (O.bindings x)
end
