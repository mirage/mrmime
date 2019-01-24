module Buffer : sig
  type t =
    | Bigstring of Bigstringaf.t
    | String of string
    | Bytes of bytes

  val weight : t -> int
  val sub : t -> int -> int -> t
end

module IOVec : sig
  type t = {buffer: Buffer.t; off: int; len: int}

  val weight : t -> int
  val length : t -> int
  val lengthv : t list -> int
  val shift : t -> int -> t
  val split : t -> int -> (t * t)
  val merge : t -> t -> t option
end

type encoder

type 'v state =
  | Flush of { continue : int -> 'v state
             ; iovecs : IOVec.t list}
  | Continue of { continue : encoder -> 'v state
                ; encoder : encoder}
  | End of 'v

type 'r k0 = (encoder -> 'r state) -> encoder -> 'r state
type ('a, 'r) k1 = 'a -> (encoder -> 'r state) -> encoder -> 'r state

val schedule_flush : (int -> encoder -> unit) -> encoder -> encoder
val continue : 'r k0

val write_char : (char, 'r) k1
val write_bytes : ?off:int -> ?len:int -> (bytes, 'r) k1
val write_string : ?off:int -> ?len:int -> (string, 'r) k1
val write_bigstring : ?off:int -> ?len:int -> (Bigstringaf.t, 'r) k1
val write_uint8 : (int, 'r) k1
val flush : 'r k0

module LE : sig
  val write_uint16 : (int, 'r) k1
  val write_uint32 : (int32, 'r) k1
  val write_uint64 : (int64, 'r) k1
end

module BE : sig
  val write_uint16 : (int, 'r) k1
  val write_uint32 : (int32, 'r) k1
  val write_uint64 : (int64, 'r) k1
end

val create : int -> encoder
