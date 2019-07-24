(** Content-type module. *)

module Type : sig
  type t = Rfc2045.ty
  (** Type of ... type. *)

  val text : t
  (** Text type. *)

  val image : t
  (** Image type. *)

  val audio : t
  (** Audio type. *)

  val video : t
  (** Video type. *)

  val application : t
  (** Application type. *)

  val message : t
  (** Message type. *)

  val multipart : t
  (** Multipart type. *)

  val ietf : string -> (t, [ `Msg of string ]) result
  (** Type defined by IETF. *)

  val extension : string -> (t, [ `Msg of string ]) result
  (** User-defined type. *)

  val pp : t Fmt.t
  (** Pretty-printer of {!t}. *)

  val compare : t -> t -> int
  (** Comparison of {!t}. *)

  val equal : t -> t -> bool
  (** Equal of {!t}. *)

  val default : t
  (** Default value of type according to RFC 2045. *)

  val is_discrete : t -> bool
  val is_multipart : t -> bool
  val is_message : t -> bool
end

module Subtype : sig
  type t = Rfc2045.subty
  (** Type of sub-type. *)

  val ietf : string -> (t, [ `Msg of string ]) result
  (** Sub-type defined by IETF. *)

  val iana : Type.t -> string -> (t, [ `Msg of string ]) result
  (** Sub-type from IANA database. Returns [Error] if sub-type
      is not a part of the IANA database. *)

  val iana_exn : Type.t -> string -> t
  val v : Type.t -> string -> t

  val extension : string -> (t, [ `Msg of string ]) result
  (** User-defined sub-type. *)

  val pp : t Fmt.t
  (** Pretty-printer of {!t}. *)

  val compare : t -> t -> int
  (** Comparison on {!t}. *)

  val equal : t -> t -> bool
  (** Equal on {!t}. *)

  val default : t
  (** Default value of sub-type acccording to RFC 2045. *)
end

module Parameters : sig
  module Map : module type of Map.Make(String)

  type key = string
  (** Type of parameter key. *)

  type value = Rfc2045.value
  (** Type of parameter value. *)

  type t = value Map.t
  (** Type of parameters. *)

  val of_list : (key * value) list -> t
  (** Make {!t} from an association list. *)

  val key : string -> (key, [ `Msg of string ]) result
  (** [key v] makes a new key (according to RFC 2045 - otherwise, it returns an
     error). *)

  val key_exn : string -> key
  val k : string -> key

  val value : string -> (value, [ `Msg of string ]) result
  (** [value v] makes a new value (according to RFC 2045 - otherwise, it returns
      an error). *)

  val value_exn : string -> value
  val v : string -> value

  val empty : t
  (** Empty parameters. *)

  val mem : key -> t -> bool
  (** [mem key t] returns true if [key] exists in [t]. Otherwise, it retunrs false. *)

  val add : key -> value -> t -> t
  (** [add key value t] adds [key] binded with [value] in [t]. *)

  val singleton : key -> value -> t
  (** [singleton key value] makes a new {!t} with [key] binded with [value]. *)

  val remove : key -> t -> t
  (** [delete key t] deletes [key] and binded value from [t]. *)

  val find : key -> t -> value option
  (** [find key t] returns value binded with [key] in [t]. *)

  val iter : (key -> value -> unit) -> t -> unit
  (** [iter f t] applies [f] on any bindings availables in [t]. *)

  val pp_key : key Fmt.t
  (** Pretty-printer of {!key}. *)

  val pp_value : value Fmt.t
  (** Pretty-printer of {!value}. *)

  val pp : t Fmt.t
  (** Pretty-printers of {!t}. *)

  val compare : t -> t -> int
  (** Comparison on {!t}. *)

  val equal : t -> t -> bool
  (** Equal on {!t}. *)

  val default : t
  (** Same as {!empty}. *)

  val to_list : t -> (key * value) list
end

type t = Rfc2045.content
(** Type of Content-Type value. *)

val default : t
(** Default Content-Type value according to RFC 2045. *)

val make : Type.t -> Subtype.t -> Parameters.t -> t
(** [make ty subty params] makes a new Content-Type value. *)

val ty : t -> Type.t
(** Return type of Content-Type value. *)

val subty : t -> Subtype.t
(** Return sub-type of Content-Type value. *)

val parameters : t -> (Parameters.key * Parameters.value) list
(** Returns parameters of Content-Type value. *)

val is_discrete : t -> bool
val is_multipart : t -> bool
val is_message : t -> bool

val with_type : t -> Type.t -> t
val with_subtype : t -> Subtype.t -> t
val with_parameter : t -> (Parameters.key * Parameters.value) -> t

(** {2 Pretty-printers.} *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

(** {2 Equals.} *)

val equal : t -> t -> bool
(** Equal of {!t}. *)

(** {2 Encoder of content-type.} *)

module Encoder : sig
  val ty : Type.t Encoder.t
  val subty : Subtype.t Encoder.t
  val content_type : t Encoder.t
end
