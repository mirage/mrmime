module Ordered = Map.Make(Number)

type field = Rfc5322.resent
type t = (Resent_field.field * Location.t) Ordered.t

let equal a b =
  let exception Diff in

  try
    Ordered.iter
      (fun n (Resent_field.Field (field_name, v), _) -> match Ordered.find_opt n b with
         | None -> raise_notrace Diff
         | Some (Resent_field.Field (field_name', v'), _) ->
           ( match Resent_field.equal field_name field_name' with
             | Some Refl.Refl ->
               let eq = Resent_field.equal_of_field_name field_name in
               if eq v v' then () else raise_notrace Diff
             | None -> raise_notrace Diff )) a ; true
  with Diff -> false

let number t =
  let open Option in
  Ordered.choose_opt t >>| fst

let length t = Ordered.cardinal t

let ( <.> ) f g = fun x -> f (g x)

let reduce
  : (Number.t * ([> field ] as 'a) * Location.t) list -> t list ->
    (t list * (Number.t * 'a * Location.t) list)
  = fun fields resents ->
    List.fold_left
      (fun (resents, rest) (n, field, loc) ->
         match field with
         | (`ResentFrom _ | `ResentDate _) as field ->
           let v = Resent_field.of_rfc5322_field field in
           (Ordered.singleton n (v, loc) :: resents), rest
         | #field as field ->
           let v = Resent_field.of_rfc5322_field field in
           (match resents with
            | last :: resents->
              (Ordered.add n (v, loc) last) :: resents

            | [] ->
              [ Ordered.singleton n (v, loc) ]), rest
         | _ -> resents, (n, field, loc) :: rest)
      (resents, []) fields
    |> fun (resents, rest) -> (resents, List.rev rest)

let pp : t Fmt.t = fun ppf t ->
  Fmt.Dump.iter_bindings
    Ordered.iter
    Fmt.(always "resent")
    Fmt.nop
    Fmt.(fun ppf (Resent_field.Field (k, v)) ->
        Dump.pair
          (using Resent_field.to_field_name Field_name.pp)
          (Resent_field.pp_of_field_name k) ppf (k, v))
    ppf (Ordered.map fst t)

module Encoder = struct
  open Encoder

  let field_name = Field_name.Encoder.field_name
  let date = Date.Encoder.date
  let mailbox = Mailbox.Encoder.mailbox
  let mailboxes = Mailbox.Encoder.mailboxes
  let addresses = Address.Encoder.addresses
  let message_id = MessageID.Encoder.message_id
  let unstructured = Unstructured.Encoder.unstructured

  let field_and_value field_value value_encoding ppf value =
    eval ppf [ !!field_name; char $ ':'; spaces 1
             ; bbox; !!value_encoding; close; new_line ] field_value value

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

  let resent ppf (_, (Resent_field.Field (field_name, v), _)) = match field_name with
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

  let resent ppf x = (list ~sep:epsilon resent) ppf (Ordered.bindings x)
end
