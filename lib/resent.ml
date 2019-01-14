type 'a field =
  | Date : Date.t field
  | From : Mailbox.t list field
  | Sender : Mailbox.t field
  | To : Address.t list field
  | Cc : Address.t list field
  | Bcc : Address.t list field
  | MessageID : MessageID.t field
  | ReplyTo : Address.t list field

type binding = B : 'a field * 'a *  Location.t -> binding

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

let with_date ?(location = Location.none) n t v =
  if has_date t then Fmt.invalid_arg "Resent value can have only one Date." ;
  { t with ordered = Ptmap.add (n :> int) (B (Date, v, location)) t.ordered
         ; date = Some n }

let with_from ?(location = Location.none) n t v =
  if has_from t then Fmt.invalid_arg "Resent value can have only on From value." ;
  { t with ordered = Ptmap.add (n :> int) (B (From, v, location)) t.ordered
         ; from = Some n }

let with_sender ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Sender, v, location)) t.ordered
         ; sender = Set.add n t.sender }

let with_to ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (To, v, location)) t.ordered
         ; too = Set.add n t.too }

let with_cc ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Cc, v, location)) t.ordered
         ; cc = Set.add n t.cc }

let with_bcc ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Bcc, v, location)) t.ordered
         ; bcc = Set.add n t.bcc }

let with_message_id ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (MessageID, v, location)) t.ordered
         ; message_id = Set.add n t.message_id }

let with_reply_to ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (ReplyTo, v, location)) t.ordered
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

let fold : (Number.t * ([> Rfc5322.field ] as 'a) * Location.t) list -> t list -> (t list * (Number.t * 'a * Location.t) list) = fun fields resents ->
  let w w index v = function
    | [] -> [ w index default v ]
    | resent :: resents -> w index resent v :: resents in

  List.fold_left
    (fun (resents, rest) field -> match field, resents with
       | (index, `ResentDate v, loc), [] ->
         [ with_date ~location:loc index default v ], rest
       | (index, `ResentDate v, loc), resent :: resents ->
         (match resent.date with
          | None -> with_date ~location:loc index resent v :: resents
          | Some _ -> with_date ~location:loc index default v :: resent :: resents), rest
       | (index, `ResentFrom v, loc), [] ->
         [ with_from ~location:loc index default v], rest
       | (index, `ResentFrom v, loc), resent :: resents ->
         (match resent.from with
          | None -> with_from ~location:loc index resent v :: resents
          | Some _ -> with_from ~location:loc index default v :: resent :: resents), rest
       | (index, `ResentSender v, loc), resents -> w (with_sender ~location:loc) index v resents, rest
       | (index, `ResentTo v, loc), resents -> w (with_to ~location:loc) index v resents, rest
       | (index, `ResentCc v, loc), resents -> w (with_cc ~location:loc) index v resents, rest
       | (index, `ResentBcc v, loc), resents -> w (with_bcc ~location:loc) index v resents, rest
       | (index, `ResentMessageID v, loc), resents -> w (with_message_id ~location:loc) index v resents, rest
       | (index, `ResentReplyTo v, loc), resents -> w (with_reply_to ~location:loc) index v resents, rest
       | (index, field, loc), resents -> resents, (index, field, loc) :: rest)
    (resents, []) fields
  |> fun (resents, fields) -> resents, List.rev fields
