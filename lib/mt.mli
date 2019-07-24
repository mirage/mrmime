type 'x stream = (unit -> 'x option)
type buffer = string * int * int

type field = Field_name.t * Unstructured.t

type part
type multipart
type 'g rng = ?g:'g -> int -> string

val rng : int array rng
(** A default random generator. *)

val part : ?content:Content.t -> ?fields:field list -> buffer stream -> part
(** [part ~content ~fields stream] makes a new part from a body stream [stream],
   [Content-*] fields, and others [fields]. If [content] is not specified, we
   use {!Content.default}. [stream] while be mapped according
   [Content-Transfer-Encoding] of [content]. *)

val multipart : rng:'g rng -> ?content:Content.t -> ?boundary:string -> ?fields:field list -> part list -> multipart
(** [multipart ~rng ~content ~boundary ~fields parts] makes a new multipart from
   a bunch of parts, specified [Content-*] fields, others [fields] and a
   specified [boundary]. If [boundary] is not specifed, we use [rng] to make a
   random boundary (we did not check that it does not appear inside [parts]). *)

val multipart_as_part : multipart -> part
(** [multipart_as_part m] compiles a multipart [m] as a part. *)

type 'x body

val simple : part body
val multi : multipart body

type t
(** Type of mail. *)

val make : Header.t -> 'x body -> 'x -> t
(** [make header kind v] makes a mail from an [header] and a element which can
   be a {!simple} part or a {!multi}part. *)

val to_stream : t -> buffer stream
(** [to_stream mail] generates a stream from a mail. *)
