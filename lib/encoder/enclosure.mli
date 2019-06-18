(** [Enclosure] module follows [Faraday] implementation (thx \@spiros). The main
   difference is: [Enclosure] uses a bounded internal [Bigstringaf.t] which can
   not be grow. Internal queue which contains [IOVec] elements can not grow too.

   By this way, we ensure that [encoder] uses a bounded memory area. Where
   [encoder] is full, any operation returns [Flush]. User should emit [IOVec]
   then to output.

   XXX(dinosaure): Faraday's cage is an enclosure. *)

module Buffer : sig
  type t =
    | Bigstring of Bigstringaf.t
    | String of string
    | Bytes of bytes

  val weight : t -> int
  (** Weight of {!t}. *)

  val sub : t -> int -> int -> t
  (** [sub t off len] does a sub operation of {!t} of [len] bytes starting at
     [off]. *)
end

module IOVec : sig
  type t = {buffer: Buffer.t; off: int; len: int}
  (** Type of IOVec. *)

  val weight : t -> int
  (** Weight of {!t}. *)

  val length : t -> int
  (** Length (in bytes) of {!t}. *)

  val lengthv : t list -> int
  (** Length (in bytes) of a list of {!t}. *)

  val shift : t -> int -> t
  (** [shift t n] shifts [n] bytes on [t]. *)

  val split : t -> int -> (t * t)
  (** [split t off] splits [t] at [off] point. *)

  val merge : t -> t -> t option
  (** [merge a b] tries to merge [a] and [b] into a new {!t}. *)
end

type encoder
(** The type of encoder. *)

val is_empty : encoder -> bool
(** [is_empty t] returns [true] if nothing is under [t]. This case appear
   afterwards a {!flush}. *)

val pp : encoder Fmt.t
(** Pretty-printer of {!encoder}. *)

val schedule_flush : (int -> encoder -> unit) -> encoder -> encoder
(** [schedule_flush f encoder] registers [f] to be called when all prior writes
   have been successfully completed. If [encoder] has no pending writes, then
   [f] will be called immediately. *)

val schedule_bigstring : encoder -> ?off:int -> ?len:int -> Bigstringaf.t -> encoder
(** [schedule_bigstring t ?off ?len a] stores a pointer to [a] into the
   serializer's internal queue. *)

val kschedule_bigstring : (encoder -> 'r) -> encoder -> ?off:int -> ?len:int -> Bigstringaf.t -> 'r
(** [kschedule_bigstring k t ?off ?len a]: [k]ontinuation-style of {!schedule_bigstring}. *)

val schedule_string : encoder -> ?off:int -> ?len:int -> string -> encoder
(** Same as {!schedule_bigstring} but for [String.t]. *)

val kschedule_string : (encoder -> 'r) -> encoder -> ?off:int -> ?len:int -> String.t -> 'r
(** [kschedule_string k t ?off ?len a]: [k]ontinuation-style of {!schedule_string}. *)

val schedule_bytes : encoder -> ?off:int -> ?len:int -> bytes -> encoder
(** Same as {!schedule_bigstring} but for [Bytes.t]. *)

val kschedule_bytes : (encoder -> 'r) -> encoder -> ?off:int -> ?len:int -> Bytes.t -> 'r
(** [kschedule_bytes k t ?off ?len a]: [k]ontinuation-style of {!schedule_bytes}. *)

val write_char : char -> encoder -> encoder
(** [write_char chr t] copies [chr] into the serializer's internal buffer. *)

val write_string : ?off:int -> ?len:int -> string -> encoder -> encoder
(** [write_string ?off ?len x t] copies [x] into the serializer's internal
   buffer. *)

val write_bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> encoder -> encoder
(** Same as {!write_string} but for {!bigstring}. *)

val write_bytes : ?off:int -> ?len:int -> bytes -> encoder -> encoder
(** Same as {!write_string} but for [Bytes.t]. *)

val write_uint8 : int -> encoder -> encoder
(** [write_uint8 x t] copies the lower 8 bits of [x] into the serializer's
   internal buffer. *)

val flush : (encoder -> 'r) -> encoder -> 'r
(** [flush k encoder] enforces to flush at this stage [encoder]. Then, when
   end-user flushes [encoder], [k] will be called with the new state of
   [encoder] drained. *)

module LE : sig
  val write_uint16 : int -> encoder -> encoder
  val write_uint32 : int32 -> encoder -> encoder
  val write_uint64 : int64 -> encoder -> encoder
end

module BE : sig
  val write_uint16 : int -> encoder -> encoder
  val write_uint32 : int32 -> encoder -> encoder
  val write_uint64 : int64 -> encoder -> encoder
end

type emitter = IOVec.t list -> int

val create : emitter:emitter -> int -> encoder
(** [create ~emitter len] creates a serializer with a fixed-length internal buffer.
    When internal buffers are full, [encoder] calls [emitter] to emit [IOVec.t list]. *)
