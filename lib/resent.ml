module Ordered = Map.Make(Number)

module Value = struct
  type t =
    | Date : Date.t -> t
    | From : Mailbox.t list -> t
    | Sender : Mailbox.t -> t
    | To : Address.t list -> t
    | Cc : Address.t list -> t
    | Bcc : Address.t list -> t
    | MessageID : MessageID.t -> t
    | ReplyTo : Address.t list -> t
    | Field : Unstructured.t -> t
    | Unsafe : Unstructured.t -> t

  let of_date x = Date x
  let of_from x = From x
  let of_sender x = Sender x
  let of_to x = To x
  let of_cc x = Cc x
  let of_bcc x = Bcc x
  let of_message_id x = MessageID x
  let of_reply_to x = ReplyTo x

  let pp ppf = function
    | Date x -> Date.pp ppf x
    | From x -> Fmt.Dump.list Mailbox.pp ppf x
    | Sender x -> Mailbox.pp ppf x
    | To x | Cc x | Bcc x | ReplyTo x -> Fmt.Dump.list Address.pp ppf x
    | MessageID x -> MessageID.pp ppf x
    | Field x | Unsafe x -> Unstructured.pp ppf x
end

let should_be_normalized field =
  let resent_fields =
    [ "Resent-Date"
    ; "Resent-From"
    ; "Resent-Sender"
    ; "Resent-To"
    ; "Resent-Cc"
    ; "Resent-Bcc"
    ; "Resent-Message-ID"
    ; "Resent-Reply-To" ] in
  let resent_fields = List.map Field.v resent_fields in
  List.exists (Field.equal field) resent_fields

let prefixed_by_resent = Field.prefixed_by "Resent"

type 'a resent_field =
  | Resent_date : Date.t resent_field
  | Resent_from : Mailbox.t list resent_field
  | Resent_sender : Mailbox.t resent_field
  | Resent_to : Address.t list resent_field
  | Resent_cc : Address.t list resent_field
  | Resent_bcc : Address.t list resent_field
  | Resent_message_id : MessageID.t resent_field
  | Resent_reply_to : Address.t list resent_field

let resent_field_to_field : type a. a resent_field -> Field.t = function
  | Resent_date -> Field.v "Resent-Date"
  | Resent_from -> Field.v "Resent-From"
  | Resent_sender -> Field.v "Resent-Sender"
  | Resent_to -> Field.v "Resent-To"
  | Resent_cc -> Field.v "Resent-Cc"
  | Resent_bcc -> Field.v "Resent-Bcc"
  | Resent_message_id -> Field.v "Resent-Message-ID"
  | Resent_reply_to -> Field.v "Resent-Reply-To"

module Info = struct
  type 'a ordered = { vs : 'a Ordered.t }

  type 'a t =
    | Normalized : { field : 'a resent_field
                   ; pp : 'a Fmt.t
                   ; prj : 'a -> Value.t } -> 'a ordered t

  let make ~field ~pp ~prj =
    Normalized { field; pp; prj; }
end

module Hmap = Hmap.Make(Info)

module Key = struct
  type 'a ordered = 'a Info.ordered

  let date : Date.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Resent_date ~pp:Date.pp ~prj:Value.of_date)
  let from : Mailbox.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Resent_from ~pp:(Fmt.Dump.list Mailbox.pp) ~prj:Value.of_from)
  let sender : Mailbox.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Resent_sender ~pp:Mailbox.pp ~prj:Value.of_sender)
  let _to : Address.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Resent_to ~pp:(Fmt.Dump.list Address.pp) ~prj:Value.of_to)
  let cc : Address.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Resent_cc ~pp:(Fmt.Dump.list Address.pp) ~prj:Value.of_cc)
  let bcc : Address.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Resent_bcc ~pp:(Fmt.Dump.list Address.pp) ~prj:Value.of_bcc)
  let message_id : MessageID.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Resent_message_id ~pp:MessageID.pp ~prj:Value.of_message_id)
  let reply_to : Address.t list ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Resent_reply_to ~pp:(Fmt.Dump.list Address.pp) ~prj:Value.of_reply_to)
end

type field =
  | Normalized : { k : 'a Hmap.key; n : Number.t } -> field
  | Field : { field : Field.t; value : Unstructured.t; n : Number.t } -> field
  | Unsafe : { field : Field.t; value : Unstructured.t; n : Number.t } -> field

type t =
  { normalized : Hmap.t
  ; v : field Location.with_location list }

let empty =
  { normalized= Hmap.empty
  ; v= [] }

let number_of_field = function
  | Normalized { n; _ } -> n
  | Field { n; _ } -> n
  | Unsafe { n; _ } -> n

let number t =
  let hi = List.fold_left (fun a x -> Number.max a (number_of_field (Location.prj x))) Number.zero t.v in
  let lo = List.fold_left (fun a x -> Number.min a (number_of_field (Location.prj x))) hi t.v in
  (lo, hi)

let location t =
  List.fold_left (fun a x -> Location.union a (Location.location x)) Location.none t.v

let ( <.> ) f g = fun x -> f (g x)

let has_date { normalized; v; } = match Hmap.find Key.date normalized with
  | Some { Info.vs } -> not (Ordered.is_empty vs)
  (* XXX(dinosaure): [vs] should have, at least, one element. *)
  | None ->
    let is_resent_date = function
      | Unsafe { field; _ } -> Field.(equal (v "Resent-Date") field)
      | Field _ | Normalized _ -> false in
    List.exists (is_resent_date <.> Location.prj) v

let has_from { normalized; v; } = match Hmap.find Key.from normalized with
  | Some { Info.vs } -> not (Ordered.is_empty vs)
  | None ->
    let is_resent_from = function
      | Unsafe { field; _ } -> Field.(equal (v "Resent-From") field)
      | Field _ | Normalized _ -> false in
    List.exists (is_resent_from <.> Location.prj) v

let value_of_field : t -> field -> Value.t = fun t -> function
  | Normalized { k; n; } ->
    let Info.Normalized { prj; _ } = Hmap.Key.info k in
    let m = Hmap.get k t.normalized in
    let v = Ordered.find n m.Info.vs in
    prj v
  | Field { value; _ } -> Value.Field value
  | Unsafe { value; _ } -> Value.Unsafe value

let get f t =
  let res = ref [] in
  let collect : field -> unit = fun x -> match x with
    | Normalized { k; _ } ->
      let Info.Normalized { field; _ } = Hmap.Key.info k in
      let v = value_of_field t x in
      if Field.equal (resent_field_to_field field) f then res := v :: !res
    | Field { field; _ } ->
      let v = value_of_field t x in
      if Field.equal field f then res := v :: !res
    | Unsafe { field; _ } ->
      let v = value_of_field t x in
      if Field.equal field f then res := v :: !res in
  List.iter (collect <.> Location.prj) t.v ; List.rev !res

let pp ppf t =
  let pp_field ppf = function
    | Normalized { k; n; } ->
      let m = Hmap.get k t.normalized in
      let Info.Normalized { field; pp; _ } = Hmap.Key.info k in
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp (resent_field_to_field field) pp (Ordered.find n m.vs)
    | Field { field; value; _ } ->
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp field Unstructured.pp value
    | Unsafe { field; value; _ } ->
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp field Unstructured.pp value in
  Fmt.(list ~sep:(const string "\n") (using Location.prj pp_field)) ppf t.v

let add k n v t =
  let vs = match Hmap.find k t with
    | Some { Info.vs } -> Ordered.add n v vs
    | None -> Ordered.singleton n v in
  Hmap.add k { Info.vs } t

let snoc x l = l @ [ x ]

(* XXX(dinosaure): this code is little bit hard but try to fold [Resent-*]
   fields. It tries to take care about [`Unsafe] [Resent-*] and [`Field]
   (prefixed by [Resent-]) values. *)

let fold
  : (Number.t * ([> Rfc5322.field ] as 'a) * Location.t) list -> t list -> (t list * (Number.t * 'a * Location.t) list)
  = fun fields resents ->
    let push ~location k n v = function
      | [] ->
        [ { normalized= Hmap.singleton k { Info.vs= Ordered.singleton n v }
          ; v= [ Location.inj ~location (Normalized { k; n; }) ] } ]
      | resent :: resents ->
        { normalized= add k n v resent.normalized
        ; v= snoc (Location.inj ~location (Normalized { k; n; })) resent.v } :: resents in

    List.fold_left
      (fun (resents, rest) field -> match field, resents with
         | (n, `ResentDate v, location), [] ->
           let t = { normalized= add Key.date n v Hmap.empty
                   ; v= [ Location.inj ~location (Normalized { k= Key.date; n }) ] } in
           [ t ], rest
         | (n, `ResentFrom v, location), [] ->
           let t = { normalized= add Key.from n v Hmap.empty
                   ; v= [ Location.inj ~location (Normalized { k= Key.from; n }) ] } in
           [ t ], rest
         | (n, `ResentDate v, location), resent :: resents ->
           let resents =
             if has_date resent
             then { normalized= add Key.date n v Hmap.empty
                  ; v= [ Location.inj ~location (Normalized { k= Key.date; n }) ] } :: resent :: resents
             else { normalized= add Key.date n v resent.normalized
                  ; v= snoc (Location.inj ~location (Normalized {k= Key.date; n }))  resent.v } :: resents in
           resents, rest
         | (n, `ResentFrom v, location), resent :: resents ->
           let resents =
             if has_date resent
             then { normalized= add Key.from n v Hmap.empty
                  ; v= [ Location.inj ~location (Normalized { k= Key.from; n }) ] } :: resent :: resents
             else { normalized= add Key.from n v resent.normalized
                  ; v= snoc (Location.inj ~location (Normalized { k= Key.from; n })) resent.v } :: resents in
           resents, rest
         | (n, `ResentSender v, location), resents ->
           push ~location Key.sender n v resents, rest
         | (n, `ResentTo v, location), resents ->
           push ~location Key._to n v resents, rest
         | (n, `ResentCc v, location), resents ->
           push ~location Key.cc n v resents, rest
         | (n, `ResentBcc v, location), resents ->
           push ~location Key.bcc n v resents, rest
         | (n, `ResentMessageID v, location), resents ->
           push ~location Key.message_id n v resents, rest
         | (n, `ResentReplyTo v, location), resents ->
           push ~location Key.reply_to n v resents, rest
         | (n, `Unsafe (field, value), location), [] ->
           let resents, rest =
             if Field.(equal (v "Resent-Date") field)
             then [ { normalized= Hmap.empty
                    ; v= [ Location.inj ~location (Unsafe { field; value; n; }) ] } ], rest
             else if Field.(equal (v "Resent-From") field)
             then [ { normalized= Hmap.empty
                    ; v= [ Location.inj ~location (Unsafe { field; value; n; }) ] } ], rest
             else [], (n, `Unsafe (field, value), location) :: rest in
           resents, rest
         | (n, `Unsafe (field, value), location), resent :: resents ->
           let resents, rest =
             if Field.(equal (v "Resent-Date") field)
             then ( if has_date resent
                    then { normalized= Hmap.empty
                         ; v= [ Location.inj ~location (Unsafe { field; value; n; }) ] } :: resent :: resents, rest
                    else { resent with v= snoc (Location.inj ~location (Unsafe { field; value; n; })) resent.v } :: resents, rest )
             else if Field.(equal (v "Resent-From") field)
             then ( if has_from resent
                    then { normalized= Hmap.empty
                         ; v= [ Location.inj ~location (Unsafe { field; value; n; }) ] } :: resent :: resents, rest
                    else { resent with v= snoc (Location.inj ~location (Unsafe { field; value; n; })) resent.v } :: resents, rest )
             else if should_be_normalized field
             then { resent with v= snoc (Location.inj ~location (Unsafe { field; value; n; })) resent.v } :: resents, rest
             else resent :: resents, (n, `Unsafe (field, value), location) :: rest in
           resents, rest
         | (n, `Field (field, value), location), [] ->
           let resents, rest =
             if prefixed_by_resent field
             then [ { normalized= Hmap.empty; v= [ Location.inj ~location (Field { field; value; n; }) ] } ], rest
             else [], (n, `Field (field, value), location) :: rest in
           resents, rest
         | (n, `Field (field, value), location), resent :: resents ->
           let resents, rest =
             if prefixed_by_resent field
             then { resent with v= snoc (Location.inj ~location (Field { field; value; n; })) resent.v } :: resents, rest
             else resent :: resents, (n, `Field (field, value), location) :: rest in
           resents, rest
         | rfc5322_field, resents ->
           (* XXX(dinosaure): we should explicitly list all of RFC5322 fields. *)
           resents, rfc5322_field :: rest)
      (resents, []) fields
    |> fun (resents, fields) -> resents, List.rev fields (* XXX(dinosaure): keep the order. *)

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let field = Field.Encoder.field
  let date = Date.Encoder.date
  let mailbox = Mailbox.Encoder.mailbox
  let mailboxes = Mailbox.Encoder.mailboxes
  let addresses = Address.Encoder.addresses
  let message_id = MessageID.Encoder.message_id
  let unstructured = Unstructured.Encoder.unstructured

  let field_and_value field_value value_encoding ppf value =
    keval ppf id [ !!field; char $ ':'; space; hov 1; !!value_encoding; close; string $ "\r\n" ] field_value value

  let resent_date = field_and_value (Field.v "Resent-Date") date
  let resent_from = field_and_value (Field.v "Resent-From") mailboxes
  let resent_sender = field_and_value (Field.v "Resent-Sender") mailbox
  let resent_to = field_and_value (Field.v "Resent-To") addresses
  let resent_cc = field_and_value (Field.v "Resent-Cc") addresses
  let resent_bcc = field_and_value (Field.v "Resent-Bcc") addresses
  let resent_message_id = field_and_value (Field.v "Resent-Message-ID") message_id
  let resent_reply_to = field_and_value (Field.v "Resent-Reply-To") addresses
  let resent_field field = field_and_value field unstructured
  let resent_unsafe field = field_and_value field unstructured

  let resent t ppf = function
    | Field { field; value; _ } -> resent_field field ppf value
    | Unsafe { field; value; _ } -> resent_unsafe field ppf value
    | Normalized { k; n; } ->
      let Normalized { field; _ } = Hmap.Key.info k in
      let m = Hmap.get k t.normalized in
      let v = Ordered.find n m.Info.vs in
      match field with
      | Resent_date -> resent_date ppf v
      | Resent_from -> resent_from ppf v
      | Resent_sender -> resent_sender ppf v
      | Resent_to -> resent_to ppf v
      | Resent_cc -> resent_cc ppf v
      | Resent_bcc -> resent_bcc ppf v
      | Resent_message_id -> resent_message_id ppf v
      | Resent_reply_to -> resent_reply_to ppf v

  let resent ppf t = (list (using Location.prj (resent t))) ppf t.v
end
