type 'a t =
  | Type : Content_type.t t
  | Encoding : Content_encoding.t t
  | ID : MessageID.t t
  | Description : Unstructured.t t
  | Field : Field_name.t -> Unstructured.t t
  (** Type of field-names defined by RFC 2045 ([Content-*] field-names). *)

val equal : 'a t -> 'b t -> ('a, 'b) Refl.t option

type 'a v =
  | Type : Content_type.t v
  | Encoding : Content_encoding.t v
  | ID : MessageID.t v
  | Unstructured : Unstructured.t v
  (** Type of values defined by RFC 2045 ([Content-*] field-values). *)

type field_name = Field_name : 'a t -> field_name
type field_value = Field_value : 'a v -> field_value
type field = Field : 'a t * 'a -> field
(** Type of fields defined by RFC 2045 ([Content-*] name with associated value). *)

val make : 'a t -> 'a -> field
(** [make field_name v] makes a [Content-*]'s field with [v]. *)

val ( $ ) : 'a t -> 'a -> field
(** Infix operator, alias of {!make}. *)

val prefixed_by_content : Field_name.t -> bool
(** [prefixed_by_content field_name] checks if [field_name] is prefixed by
   [Content-] (case-insensitive). *)

val of_field_name : Field_name.t -> field_name
(** [of_field_name field_name] casts [field_name] to a well-typed {!t}.

    @raise Invalid_argument if [field_name] is not prefixed by [Content-]
   (case-insensitive). *)

val to_field_name : 'a t -> Field_name.t
(** [to_field_name t] returns a {!Field_name.t} according RFC 2045. *)

val field_name : field -> Field_name.t
(** [field_name v] is {!Field_name.t} of [v]. *)

val field_value : 'a t -> 'a v
(** [field_value field_name] is the type-witness of [field_name]. *)

val pp_of_field_value : 'a v -> 'a Fmt.t
(** [pp_of_field_value v] is the pretty-printer of value witness of [v]. *)

val pp_of_field_name : 'a t -> 'a Fmt.t
(** [pp_of_field_value v] is the pretty-printer of value witness of [v]. *)

val equal_of_field_value : 'a v -> ('a -> 'a -> bool)
val equal_of_field_name : 'a t -> ('a -> 'a -> bool)
val field_equal : field -> field -> bool

val of_rfc2045_field : Rfc2045.field -> field
(** [of_rfc2045_field v] is well-typed field {!field} of [v]. *)
