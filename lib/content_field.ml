type 'a t =
  | Type : Content_type.t t
  | Encoding : Content_encoding.t t
  | ID : MessageID.t t
  | Description : Unstructured.t t
  | Field : Field_name.t -> Unstructured.t t

type 'a v =
  | Type : Content_type.t v
  | Encoding : Content_encoding.t v
  | ID : MessageID.t v
  | Unstructured : Unstructured.t v

type field_name = Field_name : 'a t -> field_name
type field_value = Field_value : 'a v -> field_value
type field = Field : 'a t * 'a -> field

let equal : type a b. a t -> b t -> (a, b) Refl.t option = fun a b -> match a, b with
  | Type, Type -> Some Refl.Refl
  | Encoding, Encoding -> Some Refl.Refl
  | ID, ID -> Some Refl.Refl
  | Description, Description -> Some Refl.Refl
  | Field a, Field b -> if Field_name.equal a b then Some Refl.Refl else None
  | _, _ -> None

let make : type a. a t -> a -> field =
  fun field_name field_value -> Field (field_name, field_value)

let ( $ ) = make

let prefixed_by_content = Field_name.prefixed_by "Content"

let of_field_name : Field_name.t -> field_name =
  fun field_name -> match String.lowercase_ascii (field_name :> string) with
    | "content-type" -> Field_name Type
    | "content-transfer-encoding" -> Field_name Encoding
    | "content-id" -> Field_name ID
    | "content-description" -> Field_name Description
    | _ ->
      if prefixed_by_content field_name
      then Field_name (Field field_name)
      else Fmt.invalid_arg "Invalid Content field-name: %a" Field_name.pp field_name

let to_field_name : type a. a t -> Field_name.t = function
  | Type -> Field_name.content_type
  | Encoding -> Field_name.content_encoding
  | ID -> Field_name.content_id
  | Description -> Field_name.content_description
  | Field field_name -> field_name

let field_name : field -> Field_name.t
  = fun (Field (field_name, _)) -> to_field_name field_name

let field_value : type a. a t -> a v = function
  | Type -> Type
  | Encoding -> Encoding
  | ID -> ID
  | Description -> Unstructured
  | Field _ -> Unstructured

let pp_of_field_value : type a. a v -> a Fmt.t = function
  | Type -> Content_type.pp
  | Encoding -> Content_encoding.pp
  | ID -> MessageID.pp
  | Unstructured -> Unstructured.pp

let equal_of_field_value : type a. a v -> (a -> a -> bool) = function
  | Type -> Content_type.equal
  | Encoding -> Content_encoding.equal
  | ID -> MessageID.equal
  | Unstructured -> Unstructured.equal

let pp_of_field_name : type a. a t -> a Fmt.t = fun x -> pp_of_field_value (field_value x)
let equal_of_field_name : type a. a t -> (a -> a -> bool) = fun x -> equal_of_field_value (field_value x)

let field_equal (Field (field_name, v)) (Field (field_name', v')) = match equal field_name field_name' with
  | Some Refl.Refl ->
    let eq = equal_of_field_name field_name in
    eq v v'
  | None -> false


let of_rfc2045_field : Rfc2045.field -> field = function
  | `ContentType x -> Field (Type, x)
  | `ContentEncoding x -> Field (Encoding, x)
  | `ContentID x -> Field (ID, x)
  | `ContentDescription x -> Field (Description, x)
  | `Content (field_name, x) -> Field (Field field_name, x)
