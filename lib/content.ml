type mail = [ Rfc2045.field | Rfc2045.field_version | Rfc2045.lines ]
type part = [ Rfc2045.field | Rfc2045.lines ]

let prefixed_by_content = Field.prefixed_by "Content"

module Ordered = Map.Make(Number)

module Value = struct
  type t =
    | ContentType : Content_type.t -> t
    | ContentEncoding : Content_encoding.t -> t
    | MIMEVersion : Mime_version.t -> t
    | ContentID : MessageID.t -> t
    | ContentDescription : Unstructured.t -> t
    | Field : Unstructured.t -> t
    | Unsafe : Unstructured.t -> t

  let of_content_type x = ContentType x
  let of_content_encoding x = ContentEncoding x
  let of_mime_version x = MIMEVersion x
  let of_content_id x = ContentID x
  let of_content_description x = ContentDescription x

  let pp ppf = function
    | ContentType x -> Content_type.pp ppf x
    | ContentEncoding x -> Content_encoding.pp ppf x
    | MIMEVersion x -> Mime_version.pp ppf x
    | ContentID x -> MessageID.pp ppf x
    | ContentDescription x | Field x | Unsafe x -> Unstructured.pp ppf x
end

type 'a content_field =
  | Content_type : Content_type.t content_field
  | Content_encoding : Content_encoding.t content_field
  | Mime_version : Mime_version.t content_field
  | Content_id : MessageID.t content_field
  | Content_description : Unstructured.t content_field

let field_of_content_field : type a. a content_field -> Field.t = function
  | Content_type -> Field.v "Content-Type"
  | Content_encoding -> Field.v "Content-Encoding"
  | Mime_version -> Field.v "MIME-Version"
  | Content_id -> Field.v "Content-ID"
  | Content_description -> Field.v "Content-Description"

module Info = struct
  type 'a ordered = { vs : 'a Ordered.t }

  type 'a t =
    | Normalized : { field : 'a content_field
                   ; pp : 'a Fmt.t
                   ; prj : 'a -> Value.t } -> 'a ordered t

  let make ~field ~pp ~prj =
    Normalized { field; pp; prj; }
end

module Hmap = Hmap.Make(Info)

module Key = struct
  type 'a ordered = 'a Info.ordered

  let content_type : Content_type.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Content_type ~pp:Content_type.pp ~prj:Value.of_content_type)
  let content_encoding : Content_encoding.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Content_encoding ~pp:Content_encoding.pp ~prj:Value.of_content_encoding)
  let mime_version : Mime_version.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Mime_version ~pp:Mime_version.pp ~prj:Value.of_mime_version)
  let content_id : MessageID.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Content_id ~pp:MessageID.pp ~prj:Value.of_content_id)
  let content_description : Unstructured.t ordered Hmap.key =
    Hmap.Key.create (Info.make ~field:Content_description ~pp:Unstructured.pp ~prj:Value.of_content_description)
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

let pp ppf t =
  let pp_field ppf = function
    | Normalized { k; n; } ->
      let m = Hmap.get k t.normalized in
      let Info.Normalized { field; pp; _ } = Hmap.Key.info k in
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp (field_of_content_field field) pp (Ordered.find n m.vs)
    | Field { field; value; _ } ->
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp field Unstructured.pp value
    | Unsafe { field; value; _ } ->
      Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        Field.pp field Unstructured.pp value in
  Fmt.(list ~sep:(const string "\n") (using Location.prj pp_field)) ppf t.v

let value_of_field : t -> field -> Value.t = fun t -> function
  | Normalized { k; n; } ->
    let Info.Normalized { prj; _ } = Hmap.Key.info k in
    let m = Hmap.get k t.normalized in
    let v = Ordered.find n m.Info.vs in
    prj v
  | Field { value; _ } -> Value.Field value
  | Unsafe { value; _ } -> Value.Unsafe value

let (<.>) f g = fun x -> f (g x)

let get f t =
  let res = ref [] in
  let collect : field -> unit = fun x -> match x with
    | Normalized { k; _ } ->
      let Info.Normalized { field; _ } = Hmap.Key.info k in
      let v = value_of_field t x in
      if Field.equal (field_of_content_field field) f then res := v :: !res
    | Field { field; _ } ->
      let v = value_of_field t x in
      if Field.equal field f then res := v :: !res
    | Unsafe { field; _ } ->
      let v = value_of_field t x in
      if Field.equal field f then res := v :: !res in
  List.iter (collect <.> Location.prj) t.v ; List.rev !res

let ty : t -> Content_type.Type.t = fun t ->
  match Hmap.find Key.content_type t.normalized with
  | Some m ->
    let _, v = Ordered.choose m.Info.vs in
    Content_type.ty v
  | None -> Content_type.Type.default

let subty : t -> Content_type.Subtype.t = fun t ->
  match Hmap.find Key.content_type t.normalized with
  | Some m ->
    let _, v = Ordered.choose m.Info.vs in
    Content_type.subty v
  | None -> Content_type.Subtype.default

let encoding : t -> Content_encoding.t = fun t ->
  match Hmap.find Key.content_encoding t.normalized with
  | Some m ->
    let _, v = Ordered.choose m.Info.vs in v
  | None -> Content_encoding.default

let parameters t =
  match Hmap.find Key.content_type t.normalized with
  | Some m ->
    let _, v = Ordered.choose m.Info.vs in
    Content_type.parameters v
  | None -> []

let add k n v t =
  let vs = match Hmap.find k t with
    | Some { Info.vs } -> Ordered.add n v vs
    | None -> Ordered.singleton n v in
  Hmap.add k { Info.vs } t

let snoc x l = l @ [ x ]

let fold_as_mail : ((Number.t * ([> mail ] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list) = fun fields t ->
  let add ~location k n v t =
    { normalized= add k n v t.normalized
    ; v= snoc (Location.inj ~location (Normalized { k; n; })) t.v } in
  List.fold_left
    (fun (t, rest) -> function
       | n, `ContentType v, location ->
         add ~location Key.content_type n v t, rest
       | n, `ContentEncoding v, location ->
         add ~location Key.content_encoding n v t, rest
       | n, `ContentID v, location ->
         add ~location Key.content_id n v t, rest
       | n, `ContentDescription v, location ->
         add ~location Key.content_description n v t, rest
       | n, `MIMEVersion v, location ->
         add ~location Key.mime_version n v t, rest
       | n, `Content (field, v), location ->
         { t with v= snoc (Location.inj ~location (Field { field; value= v; n; })) t.v }, rest
       | n, `Unsafe (field, v), location ->
         let t, rest =
           if prefixed_by_content field
           then { t with v= snoc (Location.inj ~location (Unsafe { field; value= v; n; })) t.v }, rest
           else t, (n, `Unsafe (field, v), location) :: rest in
         t, rest
       | n, `Field (field, v), location ->
         let t, rest =
           if prefixed_by_content field
           then { t with v= snoc (Location.inj ~location (Field { field; value= v; n; })) t.v }, rest
           else t, (n, `Field (field, v), location) :: rest in
         t, rest
       | n, field, location -> t, (n, field, location) :: rest)
    (t, []) fields
  |> fun (t, fields) -> (t, List.rev fields)


let fold_as_part : ((Number.t * ([> part ] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list) = fun fields t ->
  let add ~location k n v t =
    { normalized= add k n v t.normalized
    ; v= snoc (Location.inj ~location (Normalized { k; n; })) t.v } in
  List.fold_left
    (fun (t, rest) -> function
       | n, `ContentType v, location ->
         add ~location Key.content_type n v t, rest
       | n, `ContentEncoding v, location ->
         add ~location Key.content_encoding n v t, rest
       | n, `ContentID v, location ->
         add ~location Key.content_id n v t, rest
       | n, `ContentDescription v, location ->
         add ~location Key.content_description n v t, rest
       | n, `Content (field, v), location ->
         { t with v= snoc (Location.inj ~location (Field { field; value= v; n; })) t.v }, rest
       | n, `Unsafe (field, v), location ->
         let t, rest =
           if prefixed_by_content field
           then { t with v= snoc (Location.inj ~location (Unsafe { field; value= v; n; })) t.v }, rest
           else t, (n, `Unsafe (field, v), location) :: rest in
         t, rest
       | n, `Field (field, v), location ->
         let t, rest =
           if prefixed_by_content field
           then { t with v= snoc (Location.inj ~location (Field { field; value= v; n; })) t.v }, rest
           else t, (n, `Field (field, v), location) :: rest in
         t, rest
       | n, field, location -> t, (n, field, location) :: rest)
    (t, []) fields
  |> fun (t, fields) -> (t, List.rev fields)

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let field = Field.Encoder.field
  let content_type = Content_type.Encoder.content_type
  let content_encoding = Content_encoding.Encoder.mechanism
  let message_id = MessageID.Encoder.message_id
  let unstructured = Unstructured.Encoder.unstructured
  let mime_version = Mime_version.Encoder.mime_version

  let field_and_value field_value value_encoding ppf value =
    keval ppf id [ !!field; char $ ':'; space; hov 1; !!value_encoding; close; string $ "\r\n" ] field_value value

  let content_type = field_and_value (Field.v "Content-Type") content_type
  let content_encoding = field_and_value (Field.v "Content-Encoding") content_encoding
  let content_id = field_and_value (Field.v "Content-ID") message_id
  let content_description = field_and_value (Field.v "Content-Description") unstructured
  let mime_version = field_and_value (Field.v "MIME-Version") mime_version
  let content field = field_and_value field unstructured
  let content_unsafe field = field_and_value field unstructured
  let content_field field = field_and_value field unstructured

  let content t ppf = function
    | Field { field; value; _ } -> content_field field ppf value
    | Unsafe { field; value; _ } -> content_unsafe field ppf value
    | Normalized { k; n; } ->
      let Normalized { field; _ } = Hmap.Key.info k in
      let m = Hmap.get k t.normalized in
      let v = Ordered.find n m.Info.vs in
      match field with
      | Content_type -> content_type ppf v
      | Content_encoding -> content_encoding ppf v
      | Content_id -> content_id ppf v
      | Content_description -> content_description ppf v
      | Mime_version -> mime_version ppf v

  let content ppf t = (list (using Location.prj (content t))) ppf t.v
end
