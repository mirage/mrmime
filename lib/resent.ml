type 'a field =
  | Date : Date.t field
  | From : Mailbox.t list field
  | Sender : Mailbox.t field
  | To : Address.t list field
  | Cc : Address.t list field
  | Bcc : Address.t list field
  | MessageID : MessageID.t field
  | ReplyTo : Address.t list field

type binding = B : 'a field * 'a -> binding

module Set = Set.Make(Number)

type t =
  { date : Number.t option
  ; from : Number.t option
  ; sender : Set.t
  ; too : Set.t
  ; cc : Set.t
  ; bcc : Set.t
  ; message_id : Set.t
  ; reply_to : Set.t
  ; ordered : binding Ptmap.t }

let has_date t = Option.is_some t.date
let has_from t = Option.is_some t.from

let pp ppf _resent =
  Fmt.string ppf "#resent"

let with_date n t v =
  if has_date t then Fmt.invalid_arg "Resent value can have only one Date." ;
  { t with ordered = Ptmap.add (n :> int) (B (Date, v)) t.ordered
         ; date = Some n }

let with_from n t v =
  if has_from t then Fmt.invalid_arg "Resent value can have only on From value." ;
  { t with ordered = Ptmap.add (n :> int) (B (From, v)) t.ordered
         ; from = Some n }

let with_sender n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Sender, v)) t.ordered
         ; sender = Set.add n t.sender }

let with_to n t v =
  { t with ordered = Ptmap.add (n :> int) (B (To, v)) t.ordered
         ; too = Set.add n t.too }

let with_cc n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Cc, v)) t.ordered
         ; cc = Set.add n t.cc }

let with_bcc n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Bcc, v)) t.ordered
         ; bcc = Set.add n t.bcc }

let with_message_id n t v =
  { t with ordered = Ptmap.add (n :> int) (B (MessageID, v)) t.ordered
         ; message_id = Set.add n t.message_id }

let with_reply_to n t v =
  { t with ordered = Ptmap.add (n :> int) (B (ReplyTo, v)) t.ordered
         ; reply_to = Set.add n t.reply_to }

let default =
  { date = None
  ; from = None
  ; sender = Set.empty
  ; too = Set.empty
  ; cc = Set.empty
  ; bcc = Set.empty
  ; message_id = Set.empty
  ; reply_to = Set.empty
  ; ordered = Ptmap.empty }

let fold : (Number.t * ([> Rfc5322.field ] as 'a)) list -> t list -> (t list * (Number.t * 'a) list) = fun fields resents ->
  let w w index v = function
    | [] -> [ w index default v ]
    | resent :: resents -> w index resent v :: resents in

  List.fold_left
    (fun (resents, rest) field -> match field, resents with
       | (index, `ResentDate v), [] ->
         [ with_date index default v ], rest
       | (index, `ResentDate v), resent :: resents ->
         (match resent.date with
          | None -> with_date index resent v :: resents
          | Some _ -> with_date index default v :: resent :: resents), rest
       | (index, `ResentFrom v), [] ->
         [ with_from index default v], rest
       | (index, `ResentFrom v), resent :: resents ->
         (match resent.from with
          | None -> with_from index resent v :: resents
          | Some _ -> with_from index default v :: resent :: resents), rest
       | (index, `ResentSender v), resents -> w with_sender index v resents, rest
       | (index, `ResentTo v), resents -> w with_to index v resents, rest
       | (index, `ResentCc v), resents -> w with_cc index v resents, rest
       | (index, `ResentBcc v), resents -> w with_bcc index v resents, rest
       | (index, `ResentMessageID v), resents -> w with_message_id index v resents, rest
       | (index, `ResentReplyTo v), resents -> w with_reply_to index v resents, rest
       | (index, field), resents -> resents, (index, field) :: rest)
    (resents, []) fields
  |> fun (resents, fields) -> resents, List.rev fields
