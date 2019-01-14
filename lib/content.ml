module Field = struct
  type mail = [ Rfc2045.field | Rfc2045.field_version | Rfc2045.lines ]
  type part = [ Rfc2045.field | Rfc2045.lines ]
end

module Map = Map.Make(struct type t = string let compare a b = String.(compare (lowercase_ascii a) (lowercase_ascii b)) end)

module Set = Set.Make(Number)

type 'a field =
  | ContentType : Content_type.t field
  | ContentEncoding : Content_encoding.t field
  | MIMEVersion : Mime_version.t field
  | ContentID : MessageID.t field
  | ContentDescription : Unstructured.t field
  | Field : string -> Unstructured.t field
  | Unsafe : string -> Unstructured.t field
  | Line : string field

let pp_value_of_field : type a. a field -> a Fmt.t = function
  | ContentType -> Content_type.pp
  | ContentEncoding -> Content_encoding.pp
  | MIMEVersion -> Mime_version.pp
  | ContentID -> MessageID.pp
  | ContentDescription -> Unstructured.pp
  | Field field -> Fmt.using (fun v -> (field, v)) @@ Fmt.Dump.pair Fmt.string Unstructured.pp
  | Unsafe field -> Fmt.using (fun v -> (field, v)) @@ Fmt.Dump.pair Fmt.string Unstructured.pp
  | Line -> Utils.pp_string

type value = V : 'a field -> value
type binding = B : 'a field * 'a -> binding

module Value = struct
  type t =
    | ContentType of Content_type.t
    | ContentEncoding of Content_encoding.t
    | MIMEVersion of Mime_version.t
    | ContentID of MessageID.t
    | ContentDescription of Unstructured.t
    | Field of string * Unstructured.t
    | Unsafe of string * Unstructured.t
    | Line of string

  let pp ppf = function
    | ContentType v -> Content_type.pp ppf v
    | ContentEncoding v -> Content_encoding.pp ppf v
    | MIMEVersion v -> Mime_version.pp ppf v
    | ContentID v -> MessageID.pp ppf v
    | ContentDescription v -> Unstructured.pp ppf v
    | Field (field, v) -> Fmt.Dump.pair Fmt.string Unstructured.pp ppf (field, v)
    | Unsafe (field, v) -> Fmt.Dump.pair Fmt.string Unstructured.pp ppf (field, v)
    | Line v -> Utils.pp_string ppf v

  let of_field : type a. a field -> a -> t= fun field v -> match field with
    | ContentType -> ContentType v
    | ContentEncoding -> ContentEncoding v
    | MIMEVersion -> MIMEVersion v
    | ContentID -> ContentID v
    | ContentDescription -> ContentDescription v
    | Field field -> Field (field, v)
    | Unsafe field -> Unsafe (field, v)
    | Line -> Line v
end

type t =
  { ty : Set.t
  ; encoding : Set.t
  ; version : Set.t
  ; id : Set.t
  ; description : Set.t
  ; fields : Set.t
  ; unsafes : Set.t
  ; lines : Set.t
  ; ordered : binding Ptmap.t }

let pp_field ppf = function
  | `Content (key, value) ->
    Fmt.pf ppf "@[<hov>Content-%s:@ %a@]"
      (String.capitalize_ascii key)
      Unstructured.pp value
  | `ContentDescription v ->
    Fmt.pf ppf "@[<hov>Content-description:@ %a@]"
      Unstructured.pp v
  | `ContentType t ->
    Fmt.pf ppf "@[<hov>Content-type:@ %a@]"
      Content_type.pp t
  | `ContentEncoding t ->
    Fmt.pf ppf "@[<hov>Content-encoding:@ %a@]"
      Content_encoding.pp t
  | `ContentID t ->
    Fmt.pf ppf "@[<hov>Content-id:@ %a@]"
      MessageID.pp t
  | `MIMEVersion t ->
    Fmt.pf ppf "@[<hov>MIME-Version: %a@]"
      Mime_version.pp t
  | `Unsafe (key, value) ->
    Fmt.pf ppf "@[<hov>unsafe:%s: %a@]"
      (String.capitalize_ascii key)
      Unstructured.pp value
  | `Lines lines ->
    Fmt.pf ppf "@[<hov>line:%a]" (Fmt.hvbox Line.pp) lines

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
  Fmt.pf ppf "{ @[<hov>content-type = %a;@ \
                       content-transfer-encoding = %a;@ \
                       mime-version = %a;@ \
                       content-id = %a;@ \
                       content-description = %a;@ \
                       content-field = %a;@ \
                       unsafe = %a;@ \
                       line = %a;@] }"
    Fmt.(pp (always "content-type")) t.ty
    Fmt.(pp (always "content-encoding")) t.encoding
    Fmt.(pp (always "mime-version")) t.version
    Fmt.(pp (always "content-id")) t.id
    Fmt.(pp (always "content-description")) t.description
    Fmt.(pp (always "content-fields")) t.fields
    Fmt.(pp (always "unsafes")) t.unsafes
    Fmt.(pp (always "lines")) t.lines

let default =
  { ty = Set.empty
  ; encoding = Set.empty
  ; version = Set.empty
  ; id = Set.empty
  ; description = Set.empty
  ; fields = Set.empty
  ; unsafes = Set.empty
  ; lines = Set.empty
  ; ordered = Ptmap.empty }

let ty t =
  try
    let B (field, ty) = Ptmap.find (Set.choose t.ty :> int) t.ordered in
    match field with ContentType -> Content_type.ty ty | _ -> assert false
  with Not_found -> Content_type.Type.default

let subty t =
  try
    let B (field, ty) = Ptmap.find (Set.choose t.ty :> int) t.ordered in
    match field with ContentType -> Content_type.subty ty | _ -> assert false
  with Not_found -> Content_type.Subtype.default

let encoding t : Rfc2045.mechanism =
  try
    let B (field, encoding) = Ptmap.find (Set.choose t.encoding :> int) t.ordered in
    match field with ContentEncoding -> encoding | _ -> assert false
  with Not_found -> Content_encoding.default

let parameters t =
  let B (field, ty) = Ptmap.find (Set.choose t.ty :> int) t.ordered in
  match field with ContentType -> Content_type.parameters ty | _ -> assert false

let with_type n t v =
  { t with ordered = Ptmap.add (n :> int) (B (ContentType, v)) t.ordered
         ; ty = Set.add n t.ty }

let with_encoding n t v =
  { t with ordered = Ptmap.add (n :> int) (B (ContentEncoding, v)) t.ordered
         ; encoding = Set.add n t.encoding }

let with_version n t v =
  { t with ordered = Ptmap.add (n :> int) (B (MIMEVersion, v)) t.ordered
         ; version = Set.add n t.version }

let with_id n t v =
  { t with ordered = Ptmap.add (n :> int) (B (ContentID, v)) t.ordered
         ; id = Set.add n t.id }

let with_description n t v =
  { t with ordered = Ptmap.add (n :> int) (B (ContentDescription, v)) t.ordered
         ; description = Set.add n t.description }

let with_field n t field v =
  { t with ordered = Ptmap.add (n :> int) (B (Field field, v)) t.ordered
         ; fields = Set.add n t.fields }

let with_unsafe n t field v =
  { t with ordered = Ptmap.add (n :> int) (B (Unsafe field, v)) t.ordered
         ; unsafes = Set.add n t.unsafes }

let with_line n t v =
  { t with ordered = Ptmap.add (n :> int) (B (Line, v)) t.ordered
         ; lines = Set.add n t.lines }

let fold_as_mail : ((Number.t * ([> Field.mail ] as 'a)) list) -> t -> (t * (Number.t * 'a) list) = fun fields t ->
  List.fold_left
    (fun (t, rest) -> function
       | index, `ContentType v -> with_type index t v, rest
       | index, `ContentEncoding v -> with_encoding index t v, rest
       | index, `ContentID v -> with_id index t v, rest
       | index, `ContentDescription v -> with_description index t v, rest
       | index, `Content (field, v) -> with_field index t field v, rest
       | index, `MIMEVersion v -> with_version index t v, rest
       | index, `Unsafe (field, v) -> with_unsafe index t field v, rest
       | index, field -> t, (index, field) :: rest)
    (t, []) fields
  |> fun (t, fields) -> (t, List.rev fields)


let fold_as_part : ((Number.t * ([> Field.part ] as 'a)) list) -> t -> (t * (Number.t * 'a) list) = fun fields t ->
  List.fold_left
    (fun (t, rest) -> function
       | index, `ContentType v -> with_type index t v, rest
       | index, `ContentEncoding v -> with_encoding index t v, rest
       | index, `ContentID v -> with_id index t v, rest
       | index, `ContentDescription v -> with_description index t v, rest
       | index, `Content (field, v) -> with_field index t field v, rest
       | index, `Unsafe (field, v) -> with_unsafe index t field v, rest
       | index, field -> t, (index, field) :: rest)
    (t, []) fields
  |> fun (t, fields) -> (t, List.rev fields)
