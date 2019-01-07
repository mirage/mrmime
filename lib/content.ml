module Field = struct
  type mail = [ Rfc2045.field | Rfc2045.field_version | Rfc2045.skip ]
  type part = [ Rfc2045.field | Rfc2045.skip ]
end

module Map = Map.Make(struct type t = string let compare a b = String.(compare (lowercase_ascii a) (lowercase_ascii b)) end)

type t =
  { ty : Content_type.t
  ; encoding : Content_encoding.t
  ; version : Mime_version.t
  ; id : Msg_id.t option
  ; description : Unstructured.t option
  ; content : Unstructured.t list Map.t
  (* fields which start with ["Content-"] *)
  ; unsafe : Unstructured.t list Map.t
  (* fields noticed as [`Unsafe] *)
  ; lines : string list list
  (* lines noticed as [`Skip] *) }

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
      Msg_id.pp t
  | `MIMEVersion t ->
    Fmt.pf ppf "@[<hov>MIME-Version: %a@]"
      Mime_version.pp t
  | `Unsafe (key, value) ->
    Fmt.pf ppf "@[<hov>unsafe:%s: %a@]"
      (String.capitalize_ascii key)
      Unstructured.pp value
  | `Skip line ->
    Fmt.pf ppf "@[<hov>line:%a]" (Fmt.hvbox Line.pp) line

let pp ppf t =
  Fmt.pf ppf "{ @[<hov>content-type = %a;@ \
                       content-transfer-encoding = %a;@ \
                       mime-version = %a;@ \
                       content-id = %a;@ \
                       content-description = %a;@ \
                       content = %a;@] }"
    Content_type.pp t.ty
    Content_encoding.pp t.encoding
    Mime_version.pp t.version
    (Fmt.Dump.option Msg_id.pp) t.id
    (Fmt.Dump.option Unstructured.pp) t.description
    Fmt.(Dump.iter_bindings Map.iter (always "unsafe") (using String.capitalize_ascii string) (Dump.list Unstructured.pp)) t.content

let default =
  { ty = Content_type.default
  ; encoding = Content_encoding.default
  ; version = Mime_version.default
  ; id = None
  ; description = None
  ; content = Map.empty
  ; unsafe = Map.empty
  ; lines = [] }

let with_type t v = { t with ty = v }
let with_encoding t v = { t with encoding = v }
let with_version t v = { t with version = v }
let with_id t v = { t with id = Some v }
let with_description t v = { t with description = Some v }

let with_field t field v =
  match Map.find field t.content with
  | rest -> { t with content = Map.add field (v :: rest) t.content }
  | exception Not_found -> { t with content = Map.add field [ v ] t.content }

let with_unsafe t field v =
  match Map.find field t.unsafe with
  | rest -> { t with content = Map.add field (v :: rest) t.unsafe }
  | exception Not_found -> { t with content = Map.add field [ v ] t.unsafe }

let with_line t line =
  { t with lines = line :: t.lines }

let fold_as_mail : ([> Field.mail ] as 'a) list -> t -> (t * 'a list) = fun fields t ->
  List.fold_left
    (fun (t, rest) -> function
       | `ContentType v -> with_type t v, rest
       | `ContentEncoding v -> with_encoding t v, rest
       | `ContentID v -> with_id t v, rest
       | `ContentDescription v -> with_description t v, rest
       | `Content (field, v) -> with_field t field v, rest
       | `MIMEVersion v -> with_version t v, rest
       | `Unsafe (field, v) -> with_unsafe t field v, rest
       | `Skip line -> with_line t line, rest
       | field -> t, field :: rest)
    (t, []) fields
  |> fun (t, fields) -> (t, List.rev fields)


let fold_as_part : ([> Field.part ] as 'a) list -> t -> (t * 'a list) = fun fields t ->
  List.fold_left
    (fun (t, rest) -> function
       | `ContentType v -> with_type t v, rest
       | `ContentEncoding v -> with_encoding t v, rest
       | `ContentID v -> with_id t v, rest
       | `ContentDescription v -> with_description t v, rest
       | `Content (field, v) -> with_field t field v, rest
       | `Unsafe (field, v) -> with_unsafe t field v, rest
       | `Skip line -> with_line t line, rest
       | field -> t, field :: rest)
    (t, []) fields
  |> fun (t, fields) -> (t, List.rev fields)
