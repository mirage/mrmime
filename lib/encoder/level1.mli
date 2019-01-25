type 'v state = 'v Level0.state =
  | Flush of { continue : int -> 'v state
             ; iovecs : Level0.IOVec.t list}
  | Continue of { continue : Level0.encoder -> 'v state
                ; encoder : Level0.encoder}
  | End of 'v

type encoder

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

val new_line : 'r k0
val force_new_line : 'r k0
val if_new_line : 'r k0
val space : 'r k0
val cut : 'r k0

val hbox : 'r k0
val vbox : (int, 'r) k1
val hvbox : (int, 'r) k1
val hovbox : (int, 'r) k1
val box : (int, 'r) k1
val close_box : 'r k0

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

val create : ?margin:int -> ?new_line:string -> int -> encoder
