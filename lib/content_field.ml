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

let make : type a. a t -> a -> field =
  fun field_name field_value -> Field (field_name, field_value)

let ( $ ) = make

let prefixed_by_content = Field_name.prefixed_by "Content"

let of_field_name : Field_name.t -> field_name =
  fun field_name -> match String.lowercase_ascii (field_name :> string) with
    | "content-type" -> Field_name Type
    | "content-encoding" -> Field_name Encoding
    | "content-id" -> Field_name ID
    | "description" -> Field_name Description
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

let pp_of_field_name : type a. a t -> a Fmt.t = fun x -> pp_of_field_value (field_value x)

let of_rfc2045_field : Rfc2045.field -> field = function
  | `ContentType x -> Field (Type, x)
  | `ContentEncoding x -> Field (Encoding, x)
  | `ContentID x -> Field (ID, x)
  | `ContentDescription x -> Field (Description, x)
  | `Content (field_name, x) -> Field (Field field_name, x)
