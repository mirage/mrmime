type phrase = Rfc5322.phrase
type phrase_or_message_id = Rfc5322.phrase_or_message_id

module Field = struct
  type t = string

  let compare a b =
    let a = String.lowercase_ascii a in
    let b = String.lowercase_ascii b in
    String.compare a b

  let equal a b = compare a b = 0

  let capitalize x =
    let capitalize res idx =
      let map = function 'a' .. 'z' as chr  -> Char.unsafe_chr (Char.code chr - 32) | chr -> chr in
      Bytes.set res idx (map (Bytes.get res idx)) in
    let is_dash_or_space = function ' ' | '-' -> true | _ -> false in
    let res = Bytes.of_string x in
    for i = 0 to String.length x - 1 do
      if i > 0 && is_dash_or_space x.[i - 1]
      then capitalize res i
      else if i = 0 then capitalize res i
    done ; Bytes.unsafe_to_string res

  let canonicalize = String.lowercase_ascii

  exception Break

  let of_string x =
    try
      for i = 0 to String.length x - 1
      do if not (Rfc5322.is_ftext x.[i]) then raise Break done ;
      Ok (canonicalize x)
    with Break -> Rresult.R.error_msgf "Invalid field: %S" x

  let pp = Fmt.using capitalize Fmt.string
end

module Map = Map.Make(Field)
module Set = Set.Make(Number)

let pp_phrase = Mailbox.pp_phrase
let pp_message_id = MessageID.pp

let pp_phrase_or_message_id ppf = function
  | `Phrase phrase -> Fmt.pf ppf "(`Phrase %a)" (Fmt.hvbox pp_phrase) phrase
  | `MessageID msg_id -> Fmt.pf ppf "(`MessageID %a)" (Fmt.hvbox pp_message_id) msg_id

type 'a field =
  | Date : Date.t field
  | From : Mailbox.t list field
  | Sender : Mailbox.t field
  | ReplyTo : Address.t list field
  | To : Address.t list field
  | Cc : Address.t list field
  | Bcc : Address.t list field
  | Subject : Unstructured.t field
  | MessageID : MessageID.t field
  | InReplyTo : phrase_or_message_id list field
  | References : phrase_or_message_id list field
  | Comments : Unstructured.t field
  | Keywords : phrase list field
  | Resent : Resent.t field
  | Trace : Trace.t field
  | Field  : string -> Unstructured.t field
  | Unsafe : string -> Unstructured.t field
  | Line : string field

let date = Date
let from = From
let sender = Sender
let reply_to = ReplyTo
let too = To
let cc = Cc
let bcc = Bcc
let subject = Subject
let message_id = MessageID
let in_reply_to = InReplyTo
let references = References
let comments = Comments
let keywords = Keywords
let resent = Resent
let trace = Trace
let field field = Field field
let unsafe field = Unsafe field
let line = Line

let pp_value_of_field : type a. a field -> a Fmt.t = function
  | Date -> Date.pp
  | From -> Fmt.Dump.list Mailbox.pp
  | Sender -> Mailbox.pp
  | ReplyTo -> Fmt.Dump.list Address.pp
  | To -> Fmt.Dump.list Address.pp
  | Cc -> Fmt.Dump.list Address.pp
  | Bcc -> Fmt.Dump.list Address.pp
  | Subject -> Unstructured.pp
  | MessageID -> MessageID.pp
  | InReplyTo -> Fmt.Dump.list pp_phrase_or_message_id
  | References -> Fmt.Dump.list pp_phrase_or_message_id
  | Comments -> Unstructured.pp
  | Keywords -> Fmt.Dump.list pp_phrase
  | Resent -> Resent.pp
  | Trace -> Trace.pp
  | Field _ -> Unstructured.pp
  | Unsafe _ -> Unstructured.pp
  | Line -> Utils.pp_string

let field_to_string : type a. a field -> string = function
  | Date -> "Date"
  | From -> "From"
  | Sender -> "Sender"
  | ReplyTo -> "Reply-To"
  | To -> "To"
  | Cc -> "Cc"
  | Bcc -> "Bcc"
  | Subject -> "Subject"
  | MessageID -> "Message-ID"
  | InReplyTo -> "In-Reply-To"
  | References -> "References"
  | Comments -> "Comments"
  | Keywords -> "Keywords"
  | Resent -> "Resents"
  | Trace -> "Traces"
  | Field field -> Field.capitalize field
  | Line -> "#line"
  | Unsafe field -> Field.capitalize field

module Value = struct
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

  let pp ppf = function
    | Date v -> Date.pp ppf v
    | From v -> Fmt.Dump.list Mailbox.pp ppf v
    | Sender v -> Mailbox.pp ppf v
    | ReplyTo v -> Fmt.Dump.list Address.pp ppf v
    | To v -> Fmt.Dump.list Address.pp ppf v
    | Cc v -> Fmt.Dump.list Address.pp ppf v
    | Bcc v -> Fmt.Dump.list Address.pp ppf v
    | Subject v -> Unstructured.pp ppf v
    | MessageID v -> MessageID.pp ppf v
    | InReplyTo v -> Fmt.Dump.list pp_phrase_or_message_id ppf v
    | References v -> Fmt.Dump.list pp_phrase_or_message_id ppf v
    | Comments v -> Unstructured.pp ppf v
    | Keywords v -> Fmt.Dump.list pp_phrase ppf v
    | Resent v -> Resent.pp ppf v
    | Trace v -> Trace.pp ppf v
    | Field (field, v) -> Fmt.Dump.pair Fmt.string Unstructured.pp ppf (field, v)
    | Unsafe (field, v) -> Fmt.Dump.pair Fmt.string Unstructured.pp ppf (field, v)
    | Line v -> Utils.pp_string ppf v

  let of_field : type a. a field -> a -> t = fun field v -> match field with
    | Date -> Date v
    | From -> From v
    | Sender -> Sender v
    | ReplyTo -> ReplyTo v
    | To -> To v
    | Cc -> Cc v
    | Bcc -> Bcc v
    | Subject -> Subject v
    | MessageID -> MessageID v
    | InReplyTo -> InReplyTo v
    | References -> References v
    | Comments -> Comments v
    | Keywords-> Keywords v
    | Resent -> Resent v
    | Trace -> Trace v
    | Field field -> Field (field, v)
    | Unsafe field -> Unsafe (field, v)
    | Line -> Line v
end

type value = V : 'a field -> value
type binding = B : 'a field * 'a * Location.t -> binding

let field_of_string
  : string -> (value, Rresult.R.msg) result
  = fun x -> match String.lowercase_ascii x with
  | "date" -> Ok (V Date)
  | "from" -> Ok (V From)
  | "sender" -> Ok (V Sender)
  | "reply-to" -> Ok (V ReplyTo)
  | "to" -> Ok (V To)
  | "cc" -> Ok (V Cc)
  | "bcc" -> Ok (V Bcc)
  | "subject" -> Ok (V Subject)
  | "message-id" -> Ok (V MessageID)
  | "in-reply-to" -> Ok (V InReplyTo)
  | "references" -> Ok (V References)
  | "comments" -> Ok (V Comments)
  | "keywords" -> Ok (V Keywords)
  | "resents" -> Ok (V Resent)
  | "traces" -> Ok (V Trace)
  | field ->
    let open Rresult.R in
    Field.of_string field >>| fun field -> V (Field field)

type t =
  { date : Set.t
  ; from : Set.t
  ; sender : Set.t
  ; reply_to : Set.t
  ; too : Set.t
  ; cc : Set.t
  ; bcc : Set.t
  ; subject : Set.t
  ; message_id : Set.t
  ; in_reply_to : Set.t
  ; references : Set.t
  ; comments : Set.t
  ; keywords : Set.t
  ; resents : Resent.t list
  ; traces : Trace.t list
  ; fields : Set.t
  ; unsafes : Set.t
  ; lines : Set.t
  ; ordered : binding Ptmap.t }

let default =
  { date = Set.empty
  ; from = Set.empty
  ; sender = Set.empty
  ; reply_to = Set.empty
  ; too = Set.empty
  ; cc = Set.empty
  ; bcc = Set.empty
  ; subject = Set.empty
  ; message_id = Set.empty
  ; in_reply_to = Set.empty
  ; references = Set.empty
  ; comments = Set.empty
  ; keywords = Set.empty
  ; resents = []
  ; traces = []
  ; fields = Set.empty
  ; unsafes = Set.empty
  ; lines = Set.empty
  ; ordered = Ptmap.empty }

let pp ppf t =
  let pp_values =
    let iter computation set =
      List.iter
        (fun (x : Number.t) ->
           let B (field, value, _) = Ptmap.find (x :> int) t.ordered in
           computation (Value.of_field field value))
        (Set.elements set) in

    Fmt.Dump.iter iter Fmt.nop Value.pp in
  Fmt.pf ppf "{ @[<hov>date = %a;@ \
                       from = %a;@ \
                       sender = %a;@ \
                       reply_to = %a;@ \
                       to = %a;@ \
                       cc = %a;@ \
                       bcc = %a;@ \
                       subject = %a;@ \
                       message_id = %a;@ \
                       in_reply_to = %a;@ \
                       references = %a;@ \
                       comments = %a;@ \
                       keywords = %a;@ \
                       resents = %a;@ \
                       traces = %a;@ \
                       field = %a;@ \
                       unsafe = %a;@ \
                       lines = %a;@] }"
    pp_values t.date
    pp_values t.from
    pp_values t.sender
    pp_values t.reply_to
    pp_values t.too
    pp_values t.cc
    pp_values t.bcc
    pp_values t.subject
    pp_values t.message_id
    pp_values t.in_reply_to
    pp_values t.references
    pp_values t.comments
    pp_values t.keywords
    Fmt.(Dump.list Resent.pp) t.resents
    Fmt.(Dump.list Trace.pp) t.traces
    pp_values t.fields
    pp_values t.unsafes
    pp_values t.lines

let get : type a. a field -> t -> (a * Location.t) list = fun field t -> match field with
  | Date ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Date, v, loc) -> ((v : Date.t), loc) :: a
         | _ -> a)
      t.date []
  | From ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (From, v, loc) -> ((v : Mailbox.t list), loc) :: a
         | _ -> a)
      t.from []
  | Sender ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Sender, v, loc) -> ((v : Mailbox.t), loc) :: a
         | _ -> a)
      t.sender []
  | ReplyTo ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (ReplyTo, v, loc) -> ((v : Address.t list), loc) :: a
         | _ -> a)
      t.reply_to []
  | To ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (To, v, loc) -> ((v : Address.t list), loc) :: a
         | _ -> a)
      t.too []
  | Cc ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Cc, v, loc) -> ((v : Address.t list), loc) :: a
         | _ -> a)
      t.cc []
  | Bcc ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Bcc, v, loc) -> ((v : Address.t list), loc) :: a
         | _ -> a)
      t.bcc []
  | Subject ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Subject, v, loc) -> ((v : Unstructured.t), loc) :: a
         | _ -> a)
      t.subject []
  | MessageID ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (MessageID, v, loc) -> ((v : MessageID.t), loc) :: a
         | _ -> a)
      t.message_id []
  | InReplyTo ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (InReplyTo, v, loc) -> ((v : phrase_or_message_id list), loc) :: a
         | _ -> a)
      t.in_reply_to []
  | References ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (References, v, loc) -> ((v : phrase_or_message_id list), loc) :: a
         | _ -> a)
      t.references []
  | Comments ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Comments, v, loc) -> ((v : Unstructured.t), loc) :: a
         | _ -> a)
      t.comments []
  | Keywords ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Keywords, v, loc) -> ((v : phrase list), loc) :: a
         | _ -> a)
      t.keywords []
  | Resent -> List.map (fun resent -> resent, Location.none) t.resents
  | Trace -> List.map (fun trace -> trace, Trace.location trace) t.traces
  | Field field ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Field field', v, loc) ->
           if Field.equal field field'
           then ((v : Unstructured.t), loc) :: a
           else a
         | _ -> a) t.fields []
  | Unsafe field ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Unsafe field', v, loc) ->
           if Field.equal field field'
           then ((v : Unstructured.t), loc) :: a
           else a
         | _ -> a) t.unsafes []
  | Line ->
    Set.fold
      (fun i a -> match Ptmap.find (i :> int) t.ordered with
         | B (Line, v, loc) -> ((v : string), loc) :: a
         | _ -> a)
      t.lines []

let get_fields t =
  Set.fold
    (fun i a -> match Ptmap.find (i :> int) t.ordered with
       | B (Field field, v, loc) -> ((field, (v : Unstructured.t)), loc) :: a
       | _ -> a)
    t.fields []

let get_unsafes t =
  Set.fold
    (fun i a -> match Ptmap.find (i :> int) t.ordered with
       | B (Unsafe field, v, loc) -> ((field, (v : Unstructured.t)), loc) :: a
       | _ -> a)
    t.unsafes []

let with_date ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Date, v, location)) t.ordered
         ; date = Set.add n t.date }

let with_from ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (From, v, location)) t.ordered
         ; from = Set.add n t.from }

let with_sender ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Sender, v, location)) t.ordered
         ; sender = Set.add n t.sender }

let with_reply_to ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (ReplyTo, v, location)) t.ordered
         ; reply_to = Set.add n t.reply_to }

let with_to ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (To, v, location)) t.ordered
         ; too = Set.add n t.too }

let with_cc ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Cc, v, location)) t.ordered
         ; cc = Set.add n t.cc }

let with_bcc ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Bcc, v, location)) t.ordered
         ; bcc = Set.add n t.bcc }

let with_subject ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Subject, v, location)) t.ordered
         ; subject = Set.add n t.subject }

let with_message_id ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (MessageID, v, location)) t.ordered
         ; message_id = Set.add n t.message_id }

let with_in_reply_to ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (InReplyTo, v, location)) t.ordered
         ; in_reply_to = Set.add n t.in_reply_to }

let with_references ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (References, v, location)) t.ordered
         ; references = Set.add n t.references }

let with_comments ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Comments, v, location)) t.ordered
         ; comments = Set.add n t.comments }

let with_keywords ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Keywords, v, location)) t.ordered
         ; keywords = Set.add n t.keywords }

let with_line ?(location = Location.none) n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Line, v, location)) t.ordered
         ; lines = Set.add n t.lines }

let with_field ?(location = Location.none) n t field v =
  { t with ordered = Ptmap.add (n :> int) (B (Field field, v, location)) t.ordered
         ; fields = Set.add n t.fields }

let with_unsafe ?(location = Location.none) n t field v =
  { t with ordered = Ptmap.add (n :> int) (B (Unsafe field, v, location)) t.ordered
         ; unsafes = Set.add n t.unsafes }

let fold
  : Number.t -> (([> Rfc5322.field ] as 'a) * Location.t) list -> t -> (t * (Number.t * 'a * Location.t) list)
  = fun n fields t ->
  List.fold_left
    (fun (n, t, rest) -> function
       | `Date v, loc ->
         Number.succ n, with_date ~location:loc n t v, rest
       | `From v, loc ->
         Number.succ n, with_from ~location:loc n t v, rest
       | `Sender v, loc ->
         Number.succ n, with_sender ~location:loc n t v, rest
       | `ReplyTo v, loc ->
         Number.succ n, with_reply_to ~location:loc n t v, rest
       | `To v, loc ->
         Number.succ n, with_to ~location:loc n t v, rest
       | `Cc v, loc ->
         Number.succ n, with_cc ~location:loc n t v, rest
       | `Bcc v, loc ->
         Number.succ n, with_bcc ~location:loc n t v, rest
       | `Subject v, loc ->
         Number.succ n, with_subject ~location:loc n t v, rest
       | `MessageID v, loc ->
         Number.succ n, with_message_id ~location:loc n t v, rest
       | `InReplyTo v, loc ->
         Number.succ n, with_in_reply_to ~location:loc n t v, rest
       | `References v, loc ->
         Number.succ n, with_references ~location:loc n t v, rest
       | `Comments v, loc ->
         Number.succ n, with_comments ~location:loc n t v, rest
       | `Keywords v, loc ->
         Number.succ n, with_keywords ~location:loc n t v, rest
       | `Field (k, v), loc ->
         Number.succ n, with_field ~location:loc n t k v, rest
       | `Unsafe (k, v), loc ->
         Number.succ n, with_unsafe ~location:loc n t k v, rest
       | field, loc ->
         Number.succ n, t, (n, field, loc) :: rest)
    (n, t, []) fields
  |> fun (n, t, fields) -> (n, t, List.rev fields)
  |> fun (_, t, fields) -> Trace.fold fields []
  |> fun (traces, fields) -> Resent.fold fields []
  |> fun (resents, fields) -> ({ t with traces; resents; }, fields)
