type 'x stream = (unit -> 'x option)
type buffer = string * int * int

type field = Field_name.t * Unstructured.t

type part
type multipart
type 'g rng = ?g:'g -> int -> string

val rng : int array rng
val part : ?content:Content.t -> ?fields:field list -> buffer stream -> part
val multipart : rng:'g rng -> ?content:Content.t -> ?boundary:string -> ?fields:field list -> part list -> multipart
val multipart_as_part : multipart -> part

type 'x body

val simple : part body
val multi : multipart body

type t

val make : Header.t -> 'x body -> 'x -> t
val to_stream : t -> buffer stream
