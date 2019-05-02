type mail = [ Rfc2045.field | Rfc2045.field_version ]
type part = Rfc2045.field

module Ordered = Map.Make(Number)

type t = (Content_field.field * Location.t) Ordered.t

let empty = Ordered.empty

let find ~default predicate t =
  let exception Found in
  let res : 'a option ref = ref None in
  try
    Ordered.iter (fun _ v -> match predicate v with
        | Some r -> res := Some r ; raise Found
        | None -> ()) t ; default
  with Found -> match !res with
    | Some v -> v
    | None -> assert false

let ty : t -> Content_type.Type.t = fun t ->
  find ~default:Content_type.Type.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Type -> Some v.ty
       | _ -> None) t

let subty : t -> Content_type.Subtype.t = fun t ->
  find ~default:Content_type.Subtype.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Type -> Some v.subty
       | _ -> None) t

let encoding : t -> Content_encoding.t = fun t ->
  find ~default:Content_encoding.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Encoding -> Some v
       | _ -> None) t

let parameters : t -> Content_type.Parameters.t = fun t ->
  find ~default:Content_type.Parameters.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Type ->
         Some (Content_type.Parameters.of_list v.parameters)
       | _ -> None) t

let pp : t Fmt.t = fun ppf t ->
  Fmt.Dump.iter_bindings
    Ordered.iter
    Fmt.(always "header")
    Fmt.nop
    Fmt.(fun ppf (Content_field.Field (k, v)) ->
        Dump.pair
          (using Content_field.to_field_name Field_name.pp)
          (Content_field.pp_of_field_name k) ppf (k, v))
    ppf (Ordered.map fst t)

let reduce_as_part
  : ((Number.t * ([> part] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list)
  = fun fields content ->
    List.fold_left
      (fun (content, rest) (n, field, loc) -> match field with
         | #part as field ->
           Ordered.add n (Content_field.of_rfc2045_field field, loc) content, rest
         | field ->
           content, (n, field, loc) :: rest)
      (content, []) fields
  |> fun (content, rest) -> (content, List.rev rest)

let reduce_as_mail
  : ((Number.t * ([> mail] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list)
  = fun fields content ->
    List.fold_left
      (fun (content, rest) (n, field, loc) -> match field with
         | #part as field ->
           Ordered.add n (Content_field.of_rfc2045_field field, loc) content, rest
         | #mail -> content, rest
         (* TODO: we discard ["MIME-Version"]. *)
         | field ->
           content, (n, field, loc) :: rest)
      (content, []) fields
  |> fun (content, rest) -> (content, List.rev rest)

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let field = Field_name.Encoder.field
  let content_type = Content_type.Encoder.content_type
  let content_encoding = Content_encoding.Encoder.mechanism
  let message_id = MessageID.Encoder.message_id
  let unstructured = Unstructured.Encoder.unstructured
  let mime_version = Mime_version.Encoder.mime_version

  let field_and_value field_value value_encoding ppf value =
    keval ppf id [ !!field; char $ ':'; space; hov 1; !!value_encoding; close; string $ "\r\n" ] field_value value

  let content_type = field_and_value Field_name.content_type content_type
  let content_encoding = field_and_value Field_name.content_encoding content_encoding
  let content_id = field_and_value Field_name.content_id message_id
  let content_description = field_and_value Field_name.content_description unstructured
  let mime_version = field_and_value Field_name.mime_version mime_version
  let content field = field_and_value field unstructured
  let content_unsafe field = field_and_value field unstructured
  let content_field field = field_and_value field unstructured

  let content_as_part ppf (_, Content_field.Field (field_name, v)) = match field_name with
    | Content_field.Type -> content_type ppf v
    | Content_field.Encoding -> content_encoding ppf v
    | Content_field.ID -> content_id ppf v
    | Content_field.Description -> content_description ppf v
    | Content_field.Field field_name -> content_field field_name ppf v

  let content ppf t = (list content_as_part) ppf (Ordered.bindings t)
end
