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
type binding = B : 'a field * 'a -> binding

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
  let pp field =
    Fmt.Dump.iter_bindings
      (fun pp set ->
         List.iter
           (fun (x : Number.t) ->
              let B (field, value) = Ptmap.find (x :> int) t.ordered in
              pp (V field) (Value.of_field field value))
           (Set.elements set))
      field Fmt.nop Value.pp in
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
    Fmt.(pp (always "date")) t.date
    Fmt.(pp (always "from")) t.from
    Fmt.(pp (always "sender")) t.sender
    Fmt.(pp (always "reply-to")) t.reply_to
    Fmt.(pp (always "to")) t.too
    Fmt.(pp (always "cc")) t.cc
    Fmt.(pp (always "bcc")) t.bcc
    Fmt.(pp (always "subject")) t.subject
    Fmt.(pp (always "message-id")) t.message_id
    Fmt.(pp (always "in-reply-to")) t.in_reply_to
    Fmt.(pp (always "reference")) t.references
    Fmt.(pp (always "comment")) t.comments
    Fmt.(pp (always "keywords")) t.keywords
    Fmt.(Dump.list Resent.pp) t.resents
    Fmt.(Dump.list Trace.pp) t.traces
    Fmt.(pp (always "fields")) t.fields
    Fmt.(pp (always "unsafes")) t.unsafes
    Fmt.(pp (always "lines")) t.lines

let get : type a. a field -> t -> a list = fun field t -> match field with
  | Date -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (Date, v) -> (v : Date.t) :: a | _ -> a) t.date []
  | From -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (From, v) -> (v : Mailbox.t list) :: a | _ -> a) t.from []
  | Sender -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (Sender, v) -> (v : Mailbox.t) :: a | _ -> a) t.sender []
  | ReplyTo -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (ReplyTo, v) -> (v : Address.t list) :: a | _ -> a) t.reply_to []
  | To -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (To, v) -> (v : Address.t list) :: a | _ -> a) t.too []
  | Cc -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (Cc, v) -> (v : Address.t list) :: a | _ -> a) t.cc []
  | Bcc -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (Bcc, v) -> (v : Address.t list) :: a | _ -> a) t.bcc []
  | Subject -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (Subject, v) -> (v : Unstructured.t) :: a | _ -> a) t.subject []
  | MessageID -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (MessageID, v) -> (v : MessageID.t) :: a | _ -> a) t.message_id []
  | InReplyTo -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (InReplyTo, v) -> (v : phrase_or_message_id list) :: a | _ -> a) t.in_reply_to []
  | References -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (References, v) -> (v : phrase_or_message_id list) :: a | _ -> a) t.references []
  | Comments -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (Comments, v) -> (v : Unstructured.t) :: a | _ -> a) t.comments []
  | Keywords -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (Keywords, v) -> (v : phrase list) :: a | _ -> a) t.keywords []
  | Resent -> t.resents
  | Trace -> t.traces
  | Field field ->
    Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with
        | B (Field field', v) ->
          if Field.equal field field'
          then (v : Unstructured.t) :: a
          else a
        | _ -> a) t.fields []
  | Unsafe field ->
    Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with
        | B (Unsafe field', v) ->
          if Field.equal field field'
          then (v : Unstructured.t) :: a
          else a
        | _ -> a) t.unsafes []
  | Line -> Set.fold (fun i a -> match Ptmap.find (i :> int) t.ordered with B (Line, v) -> (v : string) :: a | _ -> a) t.lines []

let with_date n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Date, v)) t.ordered
         ; date = Set.add n t.date }

let with_from n t v =
  { t with ordered = Ptmap.add (n :> int) (B (From, v)) t.ordered
         ; from = Set.add n t.from }

let with_sender n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Sender, v)) t.ordered
         ; sender = Set.add n t.sender }

let with_reply_to n t v =
  { t with ordered = Ptmap.add (n :> int) (B (ReplyTo, v)) t.ordered
         ; reply_to = Set.add n t.reply_to }

let with_to n t v =
  { t with ordered = Ptmap.add (n :> int) (B (To, v)) t.ordered
         ; too = Set.add n t.too }

let with_cc n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Cc, v)) t.ordered
         ; cc = Set.add n t.cc }

let with_bcc n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Bcc, v)) t.ordered
         ; bcc = Set.add n t.bcc }

let with_subject n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Subject, v)) t.ordered
         ; subject = Set.add n t.subject }

let with_message_id n t v =
  { t with ordered = Ptmap.add (n :> int) (B (MessageID, v)) t.ordered
         ; message_id = Set.add n t.message_id }

let with_in_reply_to n t v =
  { t with ordered = Ptmap.add (n :> int) (B (InReplyTo, v)) t.ordered
         ; in_reply_to = Set.add n t.in_reply_to }

let with_references n t v =
  { t with ordered = Ptmap.add (n :> int) (B (References, v)) t.ordered
         ; references = Set.add n t.references }

let with_comments n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Comments, v)) t.ordered
         ; comments = Set.add n t.comments }

let with_keywords n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Keywords, v)) t.ordered
         ; keywords = Set.add n t.keywords }

let with_line n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Line, v)) t.ordered
         ; lines = Set.add n t.lines }

let with_field n t field v =
  { t with ordered = Ptmap.add (n :> int) (B (Field field, v)) t.ordered
         ; fields = Set.add n t.fields }

let with_unsafe n t field v =
  { t with ordered = Ptmap.add (n :> int) (B (Unsafe field, v)) t.ordered
         ; unsafes = Set.add n t.unsafes }

let fold : Number.t -> ([> Rfc5322.field ] as 'a) list -> t -> (t * (Number.t * 'a) list) = fun n fields t ->
  List.fold_left
    (fun (n, t, rest) -> function
       | `Date v -> Number.succ n, with_date n t v, rest
       | `From v -> Number.succ n, with_from n t v, rest
       | `Sender v -> Number.succ n, with_sender n t v, rest
       | `ReplyTo v -> Number.succ n, with_reply_to n t v, rest
       | `To v -> Number.succ n, with_to n t v, rest
       | `Cc v -> Number.succ n, with_cc n t v, rest
       | `Bcc v -> Number.succ n, with_bcc n t v, rest
       | `Subject v -> Number.succ n, with_subject n t v, rest
       | `MessageID v -> Number.succ n, with_message_id n t v, rest
       | `InReplyTo v -> Number.succ n, with_in_reply_to n t v, rest
       | `References v -> Number.succ n, with_references n t v, rest
       | `Comments v -> Number.succ n, with_comments n t v, rest
       | `Keywords v -> Number.succ n, with_keywords n t v, rest
       | `Field (k, v) -> Number.succ n, with_field n t k v, rest
       | `Unsafe (k, v) -> Number.succ n, with_unsafe n t k v, rest
       | field -> Number.succ n, t, (n, field) :: rest)
    (n, t, []) fields
  |> fun (n, t, fields) -> (n, t, List.rev fields)
  |> fun (_, t, fields) -> Trace.fold fields []
  |> fun (traces, fields) -> Resent.fold fields []
  |> fun (resents, fields) -> ({ t with traces; resents; }, fields)
