type box
type atom
type t

val pp : t Fmt.t

val box : box
val tbox : int -> box
val bbox : box

val o : box -> atom
val new_line : atom
val close : atom

val string : ?breakable:bool -> ?off:int -> ?len:int -> string -> atom
val bytes : ?breakable:bool -> ?off:int -> ?len:int -> Bytes.t -> atom
val bigstring : ?breakable:bool -> ?off:int -> ?len:int -> Bigstringaf.t -> atom

val fws : atom
val spaces : int -> atom
val break : len:int -> indent:int -> atom

val kpush : (t -> 'r) -> atom -> t -> 'r
val push : atom -> t -> t

val kflush : (t -> 'r) -> t -> 'r
val flush : t -> t

val is_empty : t -> bool

val create : ?margin:int -> ?new_line:string -> emitter:Enclosure.emitter -> int -> t
