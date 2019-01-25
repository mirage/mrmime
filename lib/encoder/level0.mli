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

type 'v state =
  | Flush of { continue : int -> 'v state
             ; iovecs : IOVec.t list}
  (** [Flush] state gives to the user [iovecs] to emit them to an output. Then,
     user needs to call [continue] with how many byte(s) he was able to emit. *)
  | Continue of { continue : encoder -> 'v state
                ; encoder : encoder}
  (** [Continue] state gives to the user the current statut of [encoder] and the
     {i continuation} function. User does not need to do anything else than call
     [continue] with [encoder]. *)
  | End of 'v
  (** [End] state gives to the user returned value by the continuation. *)

type 'r k0 = (encoder -> 'r state) -> encoder -> 'r state
(** Type of a continuation with [0] arity. *)

type ('a, 'r) k1 = 'a -> (encoder -> 'r state) -> encoder -> 'r state
(** Type of a continuation with [1] arity. *)

val schedule_flush : (int -> encoder -> unit) -> encoder -> encoder
(** [schedule_flush f encoder] registers [f] to be called when all prior writes
   have been successfully completed. If [encoder] has no pending writes, then
   [f] will be called immediately. *)

val continue : 'r k0
(** [continue k encoder] returns a ['v state] with the continuation [k] - when
   ['v state] will be evaluated, [k] will be [continue] of [Continue]
   constructor. *)

val write_char : (char, 'r) k1
(** [write_char chr k t] copies [chr] into the serializer's internal buffer. *)

val write_string : ?off:int -> ?len:int -> (string, 'r) k1
(** [write_string ?off ?len x k t] copies [x] into the serializer's internal
   buffer. When the copy is finalized, [k] is called. It returns a ['v state]
   which tells you if the encoding is done, if we need to flush contents or stop
   at a {i continue} step. *)

val write_bigstring : ?off:int -> ?len:int -> (Bigstringaf.t, 'r) k1
(** Same as {!write_string} but for {!bigstring}. *)

val write_bytes : ?off:int -> ?len:int -> (bytes, 'r) k1
(** Same as {!write_string} but for [Bytes.t]. *)

val write_uint8 : (int, 'r) k1
(** [write_uint8 x k t] copies the lower 8 bits of [x] into the serializer's
   internal buffer. *)

val flush : 'r k0
(** [flush k encoder] enforces to flush at this stage [encoder]. Then, when
   end-user flushes [encoder], [k] will be called. *)

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
(** [create len] creates a serializer with a fixed-length internal buffer. *)
