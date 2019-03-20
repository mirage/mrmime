type phrase = Rfc5322.phrase
type phrase_or_message_id = Rfc5322.phrase_or_message_id

let pp_phrase = Mailbox.pp_phrase
let pp_message_id = MessageID.pp

let pp_phrase_or_message_id ppf = function
  | `Phrase phrase -> Fmt.pf ppf "(`Phrase %a)" (Fmt.hvbox pp_phrase) phrase
  | `MessageID msg_id -> Fmt.pf ppf "(`MessageID %a)" (Fmt.hvbox pp_message_id) msg_id

module Ordered = Map.Make(Number)

module Value = struct
  type t =
    | Field : Unstructured.t -> t
    | Unsafe : Unstructured.t -> t
    | Resent : Resent.Value.t list -> t
    | Trace : Trace.Value.t list -> t
    | Date : Date.t -> t
    | From : Mailbox.t list -> t
    | Sender : Mailbox.t -> t
    | ReplyTo : Address.t list -> t
    | To : Address.t list -> t
    | Cc : Address.t list -> t
    | Bcc : Address.t list -> t
    | Subject : Unstructured.t -> t
    | MessageID : MessageID.t -> t
    | InReplyTo : phrase_or_message_id list -> t
    | References : phrase_or_message_id list -> t
    | Comments : Unstructured.t -> t
    | Keywords : phrase list -> t

  let of_date x = Date x
  let of_from x = From x
  let of_sender x = Sender x
  let of_reply_to x = ReplyTo x
  let of_to x = To x
  let of_cc x = Cc x
  let of_bcc x = Bcc x
  let of_subject x = Subject x
  let of_message_id x = MessageID x
  let of_in_reply_to x = InReplyTo x
  let of_references x = References x
  let of_comments x = Comments x
  let of_keywords x = Keywords x

  let pp ppf = function
    | Field x | Unsafe x | Subject x | Comments x -> Unstructured.pp ppf x
    | Date x -> Date.pp ppf x
    | Sender x -> Mailbox.pp ppf x
    | ReplyTo x | To x | Cc x | Bcc x -> Fmt.Dump.list Address.pp ppf x
    | From x -> Fmt.Dump.list Mailbox.pp ppf x
    | MessageID x -> MessageID.pp ppf x
    | InReplyTo x | References x -> Fmt.Dump.list pp_phrase_or_message_id ppf x
    | Keywords x -> Fmt.Dump.list pp_phrase ppf x
    | Resent x -> Fmt.Dump.list Resent.Value.pp ppf x
    | Trace x -> Fmt.Dump.list Trace.Value.pp ppf x
end

module Info = struct
  type 'a ordered = { vs : 'a Ordered.t }

  type 'a t =
    | Normalized : { field : Field.t
                   ; pp : 'a Fmt.t
                   ; prj : 'a -> Value.t } -> 'a ordered t

  let make ~field ~pp ~prj =
    Normalized { field; pp; prj; }
end

module Hmap = Hmap.Make(Info)

module Key = struct
  type 'a ordered = 'a Info.ordered

  let date : Date.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Date") ~pp:Date.pp ~prj:Value.of_date)
  let from : Mailbox.t list Info.ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "From") ~pp:(Fmt.Dump.list Mailbox.pp) ~prj:Value.of_from)
  let sender : Mailbox.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Sender") ~pp:Mailbox.pp ~prj:Value.of_sender)
  let reply_to : Address.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Reply-To") ~pp:(Fmt.Dump.list Address.pp) ~prj:Value.of_reply_to)
  let _to : Address.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "To") ~pp:(Fmt.Dump.list Address.pp) ~prj:Value.of_to)
  let cc : Address.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Cc") ~pp:(Fmt.Dump.list Address.pp) ~prj:Value.of_cc)
  let bcc : Address.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Bcc") ~pp:(Fmt.Dump.list Address.pp) ~prj:Value.of_bcc)
  let subject : Unstructured.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Subject") ~pp:Unstructured.pp ~prj:Value.of_subject)
  let message_id : MessageID.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Message-ID") ~pp:MessageID.pp ~prj:Value.of_message_id)
  let in_reply_to : phrase_or_message_id list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "In-Reply-To") ~pp:(Fmt.Dump.list pp_phrase_or_message_id) ~prj:Value.of_in_reply_to)
  let references : phrase_or_message_id list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "References") ~pp:(Fmt.Dump.list pp_phrase_or_message_id) ~prj:Value.of_references)
  let comments : Unstructured.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Comments") ~pp:Unstructured.pp ~prj:Value.of_comments)
  let keywords : phrase list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:(Field.v "Keywords") ~pp:(Fmt.Dump.list pp_phrase) ~prj:Value.of_keywords)

  let field_of_key (Info.Normalized { field; _ }) = field
  let pp_of_key (Info.Normalized { pp; _ }) = pp
end

type field =
  | Normalized : { k : 'a Hmap.key; n : Number.t } -> field
  | Resent : Resent.t -> field
  | Trace : Trace.t -> field
  | Field : { field : Field.t; value : Unstructured.t; n : Number.t } -> field
  | Unsafe : { field : Field.t; value : Unstructured.t; n : Number.t } -> field
  | Line : (string * Number.t) -> field

let number_of_field = function
  | Normalized { n; _ } -> n
  | Resent x -> fst (Resent.number x)
  | Trace x -> Trace.number x
  | Field { n; _ } -> n
  | Unsafe { n; _ } -> n
  | Line (_, n) -> n

type t =
  { normalized : Hmap.t
  ; v : field Location.w list }

let empty =
  { normalized= Hmap.empty
  ; v= [] }

let snoc x l = l @ [ x ]

let pp ppf t =
  let pp_field ppf = function
    | Normalized { k; n; } ->
      let m = Hmap.get k t.normalized in
      let Info.Normalized { field; pp; _ } = Hmap.Key.info k in
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp field pp (Ordered.find n m.vs)
    | Resent x -> Resent.pp ppf x
    | Trace x -> Trace.pp ppf x
    | Field { field; value; _ } ->
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp field Unstructured.pp value
    | Unsafe { field; value; _ } ->
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp field Unstructured.pp value
    | Line (line, _) ->
      Fmt.pf ppf "@[%S@]" line in
  Fmt.(list ~sep:(const string "\n") (using Location.prj pp_field)) ppf t.v

let value_of_field : t -> field Location.w -> Value.t * Location.t = fun t x ->
  let location = Location.location x in
  let x = Location.prj x in
  match x with
  | Normalized { k; n; } ->
    let (Info.Normalized { prj; _ }) = Hmap.Key.info k in
    let m = Hmap.get k t.normalized in
    let v = Ordered.find n m.Info.vs in
    prj v, location
  | Field { value; _ } -> Value.Field value, location
  | Unsafe { value; _ } -> Value.Unsafe value, location

  (* XXX(dinosaure): see {!get} function which never calls [value_of_field] for
     these cases. *)

  | Resent _ -> assert false
  | Trace _ -> assert false
  | Line _ -> assert false

let ( <.> ) f g = fun x -> f (g x)

let get f t =
  let res = ref [] in
  let collect : field Location.w -> unit = fun x -> match Location.prj x with
    | Normalized { k; _ } ->
      let Info.Normalized { field; _ } = Hmap.Key.info k in
      let v = value_of_field t x in
      if Field.equal field f then res := v :: !res
    | Field { field; _ } ->
      let v = value_of_field t x in
      if Field.equal field f then res := v :: !res
    | Unsafe { field; _ } ->
      let v = value_of_field t x in
      if Field.equal field f then res := v :: !res
    | Resent v ->
      ( try let xs = Resent.get f v in res := (Value.Resent xs, Location.location x) :: !res
        with Not_found -> () )
    | Trace v ->
      ( try let xs = Trace.get f v in res := (Value.Trace xs, Location.location x) :: !res
        with Not_found -> () )
    | Line _ -> () in
  List.iter collect t.v ; List.rev !res

let get_first f t = match get f t with
  | x :: _ -> x
  | [] -> raise Not_found

(* XXX(dinosaure): utils to [fold]. *)

let add k n v t =
  let vs = match Hmap.find k t with
    | Some { Info.vs } -> Ordered.add n v vs
    | None -> Ordered.singleton n v in
  Hmap.add k { Info.vs } t

(* TODO: fold [Resent] and [Trace] before common fields. *)

let fold
  : Number.t -> (([> Rfc5322.field ] as 'a) * Location.t) list -> t -> (t * (Number.t * 'a * Location.t) list)
  = fun n fields t ->
    let trace x = Location.inj ~location:(Trace.location x) (Trace x) in
    let resent x = Location.inj ~location:(Resent.location x) (Resent x) in
    let sort t = { t with v= List.sort (fun a b -> Number.compare (number_of_field (Location.prj a)) (number_of_field (Location.prj b))) t.v } in
    List.fold_left
      (fun (n, t, rest) -> function
         | `Date v, location ->
           let t = { t with normalized= add Key.date n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.date; n })) t.v } in
           (Number.succ n, t, rest)
         | `From v, location ->
           let t = { t with normalized= add Key.from n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.from; n })) t.v } in
           (Number.succ n, t, rest)
         | `Sender v, location ->
           let t = { t with normalized= add Key.sender n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.sender; n })) t.v } in
           (Number.succ n, t, rest)
         | `ReplyTo v, location ->
           let t = { t with normalized= add Key.reply_to n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.reply_to; n })) t.v } in
           (Number.succ n, t, rest)
         | `To v, location ->
           let t = { t with normalized= add Key._to n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key._to; n })) t.v } in
           (Number.succ n, t, rest)
         | `Cc v, location ->
           let t = { t with normalized= add Key.cc n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.cc; n })) t.v } in
           (Number.succ n, t, rest)
         | `Bcc v, location ->
           let t = { t with normalized= add Key.bcc n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.bcc; n })) t.v } in
           (Number.succ n, t, rest)
         | `Subject v, location ->
           let t = { t with normalized= add Key.subject n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.subject; n })) t.v } in
           (Number.succ n, t, rest)
         | `MessageID v, location ->
           let t = { t with normalized= add Key.message_id n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.message_id; n })) t.v } in
           (Number.succ n, t, rest)
         | `InReplyTo v, location ->
           let t = { t with normalized= add Key.in_reply_to n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.in_reply_to; n })) t.v } in
           (Number.succ n, t, rest)
         | `References v, location ->
           let t = { t with normalized= add Key.references n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.references; n })) t.v } in
           (Number.succ n, t, rest)
         | `Comments v, location ->
           let t = { t with normalized= add Key.comments n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.comments; n })) t.v } in
           (Number.succ n, t, rest)
         | `Keywords v, location ->
           let t = { t with normalized= add Key.keywords n v t.normalized } in
           let t = { t with v= snoc (Location.inj ~location (Normalized { k= Key.keywords; n })) t.v } in
           (Number.succ n, t, rest)
         | `Field (k, v), location ->
           let t = { t with v= snoc (Location.inj ~location (Field { field= k; value= v; n })) t.v } in
           (Number.succ n, t, rest)
         | `Unsafe (k, v), location ->
           let t = { t with v= snoc (Location.inj ~location (Unsafe { field= k; value= v; n })) t.v } in
           (Number.succ n, t, rest)
         | `Lines lines, _ ->
           let n, lines = List.fold_left (fun (n, lines) (line, location) -> Number.succ n, Location.inj ~location (Line (line, n)) :: lines) (n, []) lines in
           n, { t with v= lines @ t.v }, rest
         | field, loc ->
           Number.succ n, t, (n, field, loc) :: rest)
      (n, t, []) fields
    |> fun (n, t, fields) -> (n, t, List.rev fields) (* XXX(dinosaure): keep order. *)
    |> fun (_, t, fields) -> Trace.fold fields []
    |> fun (traces, fields) -> Resent.fold fields []
    |> fun (resents, fields) ->
    (sort { t with v= List.map trace traces
                      @ List.map resent resents
                      @ t.v },
     fields)
