type encoder
type 'r k0 = (encoder -> 'r Encoder.state) -> encoder -> 'r Encoder.state

type ('a, 'r) k1 = 'a -> (encoder -> 'r Encoder.state) -> encoder -> 'r Encoder.state

val write_char : (char, 'r) k1
val write_bytes : (bytes, 'r) k1
val write_string : (string, 'r) k1
val write_bigstring : (Bigstringaf.t, 'r) k1
val write_uint8 : (int, 'r) k1
val new_line : 'r k0
val flush : 'r k0
val force_new_line : 'r k0
val if_new_line : 'r k0
val space : 'r k0
val cut : 'r k0
val hbox : 'r k0
val vbox : (int, 'r) k1
val hvbox : (int, 'r) k1
val hovbox : (int, 'r) k1
val box : (int, 'r) k1

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
