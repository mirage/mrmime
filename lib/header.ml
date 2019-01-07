type field = Rfc5322.field

type phrase = Rfc5322.phrase
type phrase_or_msg_id = Rfc5322.phrase_or_msg_id

module Map = Map.Make(struct type t = string let compare a b = String.(compare (lowercase_ascii a) (lowercase_ascii b)) end)

type t =
  { date : Date.t option
  ; from : Mailbox.t list
  ; sender : Mailbox.t option
  ; reply_to : Address.t list
  ; too : Address.t list
  ; cc : Address.t list
  ; bcc : Address.t list
  ; subject : Unstructured.t option
  ; msg_id : Msg_id.t option
  ; in_reply_to : phrase_or_msg_id list
  ; references : phrase_or_msg_id list
  ; comments : Unstructured.t list
  ; keywords : phrase list list
  ; resents : Resent.t list
  ; traces : Trace.t list
  ; field : Unstructured.t list Map.t
  ; unsafe : Unstructured.t list Map.t
  ; lines : string list list }

let pp_phrase = Mailbox.pp_phrase
let pp_msg_id = Msg_id.pp

let pp_phrase_or_msg_id ppf = function
  | `Phrase phrase -> Fmt.pf ppf "(`Phrase %a)" (Fmt.hvbox pp_phrase) phrase
  | `MsgID msg_id -> Fmt.pf ppf "(`MsgID %a)" (Fmt.hvbox pp_msg_id) msg_id

let pp ppf t =
  Fmt.pf ppf "{ @[<hov>date = %a;@ \
                       from = %a;@ \
                       sender = %a;@ \
                       reply_to = %a;@ \
                       to = %a;@ \
                       cc = %a;@ \
                       bcc = %a;@ \
                       subject = %a;@ \
                       msg_id = %a;@ \
                       in_reply_to = %a;@ \
                       references = %a;@ \
                       comments = %a;@ \
                       keywords = %a;@ \
                       resents = %a;@ \
                       traces = %a;@ \
                       field = %a;@ \
                       unsafe = %a;@ \
                       lines = %a;@] }"
    (Fmt.Dump.option Date.pp) t.date
    (Fmt.Dump.list Mailbox.pp) t.from
    (Fmt.Dump.option Mailbox.pp) t.sender
    (Fmt.Dump.list Address.pp) t.reply_to
    (Fmt.Dump.list Address.pp) t.too
    (Fmt.Dump.list Address.pp) t.cc
    (Fmt.Dump.list Address.pp) t.bcc
    (Fmt.Dump.option Unstructured.pp) t.subject
    (Fmt.Dump.option Msg_id.pp) t.msg_id
    (Fmt.Dump.list pp_phrase_or_msg_id) t.in_reply_to
    (Fmt.Dump.list pp_phrase_or_msg_id) t.references
    (Fmt.Dump.list Unstructured.pp) t.comments
    Fmt.Dump.(list (list pp_phrase)) t.keywords
    (Fmt.Dump.list Resent.pp) t.resents
    (Fmt.Dump.list Trace.pp) t.traces
    Fmt.(Dump.iter_bindings Map.iter (always "fields") string (Dump.list Unstructured.pp)) t.field
    Fmt.(Dump.iter_bindings Map.iter (always "unsafes") string (Dump.list Unstructured.pp)) t.unsafe
    Fmt.Dump.(list (list Line.pp)) t.lines

let default =
  { date = None
  ; from = []
  ; sender = None
  ; reply_to = []
  ; too = []
  ; cc = []
  ; bcc = []
  ; subject = None
  ; msg_id = None
  ; in_reply_to = []
  ; references = []
  ; comments = []
  ; keywords = []
  ; resents = []
  ; traces = []
  ; field = Map.empty
  ; unsafe = Map.empty
  ; lines = [] }

let with_date t v = { t with date = Some v }
let with_from t v = { t with from = v @ t.from }
let with_sender t v = { t with sender = Some v }
let with_reply_to t v = { t with reply_to = v @ t.reply_to }
let with_to t v = { t with too = v @ t.too }
let with_cc t v = { t with cc = v @ t.cc }
let with_bcc t v = { t with bcc = v @ t.bcc }
let with_subject t v = { t with subject = Some v }
let with_msg_id t v = { t with msg_id = Some v }
let with_in_reply_to t v = { t with in_reply_to = v @ t.in_reply_to }
let with_references t v = { t with references = v @ t.references }
let with_comments t v = { t with comments = v :: t.comments }
let with_keywords t v = { t with keywords = v :: t.keywords }
let with_line t line = { t with lines = line :: t.lines }

let with_field t k v =
  match Map.find k t.field with
  | rest -> { t with field = Map.add k (v :: rest) t.field }
  | exception Not_found -> { t with field = Map.add k [ v ] t.field }

let with_unsafe t k v =
  match Map.find k t.unsafe with
  | rest -> { t with unsafe = Map.add k (v :: rest) t.unsafe }
  | exception Not_found -> { t with unsafe = Map.add k [ v ] t.unsafe }

let fold : ([> field ] as 'a) list -> t -> (t * 'a list) = fun fields t ->
  List.fold_left
    (fun (t, rest) -> function
       | `Date v -> with_date t v, rest
       | `From v -> with_from t v, rest
       | `Sender v -> with_sender t v, rest
       | `ReplyTo v -> with_reply_to t v, rest
       | `To v -> with_to t v, rest
       | `Cc v -> with_cc t v, rest
       | `Bcc v -> with_bcc t v, rest
       | `Subject v -> with_subject t v, rest
       | `MessageID v -> with_msg_id t v, rest
       | `InReplyTo v -> with_in_reply_to t v, rest
       | `References v -> with_references t v, rest
       | `Comments v -> with_comments t v, rest
       | `Keywords v -> with_keywords t v, rest
       | `Field (k, v) -> with_field t k v, rest
       | `Unsafe (k, v) -> with_unsafe t k v, rest
       | `Skip line -> with_line t line, rest
       | field -> t, field :: rest)
    (t, []) fields
  |> fun (t, fields) -> (t, List.rev fields)
  |> fun (t, fields) -> Trace.fold fields []
  |> fun (traces, fields) -> Resent.fold fields []
  |> fun (resents, fields) -> ({ t with traces; resents; }, fields)
