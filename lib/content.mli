type mail = [ Rfc2045.field | Rfc2045.field_version ]
type part = Rfc2045.field
type t

val default : t
(** Default [Content-*] fields according RFC 2045 - which contains at least a
   [Content-type] field with {!Content_type.Type.default} as type and
   {!Content_type.Subtype.default} as subtype. *)

val make : ?encoding:Content_encoding.t -> ?id:MessageID.t -> Content_type.t -> t
(** [make ~encoding ~id content_type] makes a [content] value which contains
   [Content-Transfer-Encoding] with [encoding] value, [Content-ID] with [id] and
   [Content-Type] field with [content_type] value. *)

val number : t -> Number.t option
(** [number content] returns the last order of the last [field] added into
   [content]. *)

val length : t -> int
(** [length content] returns how many [Content-*] fields [content] has. *)

val empty : t
(** [empty] is an empty content which does not have any default values. *)

val add : Content_field.field -> t -> t
(** [add field content] adds [field] into [content]. *)

val add_or_replace : Content_field.field -> t -> t
(** [add_or_replace field content] adds [field] if it does not exist in
   [content] or replace older one by the new one. *)

val merge : (Content_field.field option -> Content_field.field option -> Content_field.field option) -> t -> t -> t
(** [merge f a b] computes a map whose fields is a subset of fields of [a] and
   of [b]. The presence of each such field, and the corresponding value, is
   determined with the function [f]. *)

val add_parameter : key:Content_type.Parameters.key -> value:Content_type.Parameters.value -> t -> t
(** [add_parameter ~key ~value content] directly adds parameter to the
   [Content-Type] field. If the [Content-Type] does not exist in [content], we
   add a default one. *)

val ( & ) : Content_field.field -> t -> t
(** Alias of {!add}. *)

val ty : t -> Content_type.Type.t
(** [ty content] returns type of the [Content-Type] field. If it does not exist,
   it returns {!Content_type.Type.default}. *)

val subty : t -> Content_type.Subtype.t
(** [subty content] returns subtype of the [Content-type] field. If it does not
   exist, it returns {!Content_type.Subtype.default}. *)

val encoding : t -> Content_encoding.t
(** [encoding content] returns value of the [Content-Encoding] field. If it does
   not exist, it returns {!Content_encoding.default}. *)

val parameters : t -> Content_type.Parameters.t
(** [parameters content] returns parameters of the [Content-Type] field. *)

val is_discrete : t -> bool
(** [is_discrete content] returns [true] if the type of the [Content-Type] field
   is {i discrete} (eg. {!Content_type.Type.t}). If [Content-Type] does not
   exist, it returns [true]. In any other case, it returns [false]. *)

val is_multipart : t -> bool
(** [is_multipart content] returns [true] if the type of the [Content-Type] field
   is [multipart]. If [Content-Type] does not exist or type is not [multipart], it
   returns [false]. *)

val is_message : t -> bool
(** [is_message content] returns [true] if the type of the [Content-Type] field
   is [message]. If [Content-Type] does not exist or type is not [message], it
   returns [false]. *)

val boundary : t -> Rfc2045.value option
(** [boundary content] introspect [content] to return existing [boundary]
   parameter of [Content-Type] field. If [Content-Type] or [boundary] parameter
   does not exist, it returns [None]. *)

(** {2 Equals.} *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)

(** {2 Pretty-printers.} *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

val reduce_as_part : ((Number.t * ([> part] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list)
val reduce_as_mail : ((Number.t * ([> mail] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list)

(** {2 Encoder of Content-* fields.} *)

module Encoder : sig
  val content : t Encoder.t
end

