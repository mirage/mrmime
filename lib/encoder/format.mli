(** [Fe] is Fancy Level0.

    Goal of this module is to provide a Format-like module to be able to write
   description of how to encode OCaml value on top of {!Level0}. [Fe] wants a
   writer at this stage to call it when it retrieves {!Level0.Flush} case.
   User, at this stage did not manage {i syscall} to emit bytes. User provides a
   function ({!writer}) to emit bytes and [Fe] will call it when it's needed. *)

module IOVec = Level0.IOVec

module type ENCODER = sig
  type encoder

  type 'v state = 'v Level0.state =
    | Flush of
        { continue: int -> 'v state
        ; iovecs: IOVec.t list }
    | Continue of
        { continue: Level0.encoder -> 'v state
        ; encoder: Level0.encoder }
    | End of 'v

  val flush : (encoder -> 'v state) -> encoder -> 'v state
  val continue : (encoder -> 'v state) -> encoder -> 'v state

  val write_string :
       ?off:int
    -> ?len:int
    -> string
    -> (encoder -> 'v state)
    -> encoder
    -> 'v state

  val write_char : char -> (encoder -> 'v state) -> encoder -> 'v state
  val write_uint8 : int -> (encoder -> 'v state) -> encoder -> 'v state

  val write_bytes :
       ?off:int
    -> ?len:int
    -> bytes
    -> (encoder -> 'v state)
    -> encoder
    -> 'v state

  val write_bigstring :
       ?off:int
    -> ?len:int
    -> Bigstringaf.t
    -> (encoder -> 'v state)
    -> encoder
    -> 'v state

  val schedule_flush : (int -> encoder -> unit) -> encoder -> encoder

  val new_line : (encoder -> 'v state) -> encoder -> 'v state
  val force_new_line : (encoder -> 'v state) -> encoder -> 'v state
  val if_new_line : (encoder -> 'v state) -> encoder -> 'v state
  val space : (encoder -> 'v state) -> encoder -> 'v state
  val cut : (encoder -> 'v state) -> encoder -> 'v state
  val hbox : (encoder -> 'v state) -> encoder -> 'v state
  val vbox : int -> (encoder -> 'v state) -> encoder -> 'v state
  val hvbox : int -> (encoder -> 'v state) -> encoder -> 'v state
  val hovbox : int -> (encoder -> 'v state) -> encoder -> 'v state
  val box : int -> (encoder -> 'v state) -> encoder -> 'v state
  val close_box : (encoder -> 'v state) -> encoder -> 'v state

  module LE : sig
    val write_uint16 : int -> (encoder -> 'v state) -> encoder -> 'v state
    val write_uint32 : int32 -> (encoder -> 'v state) -> encoder -> 'v state
    val write_uint64 : int64 -> (encoder -> 'v state) -> encoder -> 'v state
  end

  module BE : sig
    val write_uint16 : int -> (encoder -> 'v state) -> encoder -> 'v state
    val write_uint32 : int32 -> (encoder -> 'v state) -> encoder -> 'v state
    val write_uint64 : int64 -> (encoder -> 'v state) -> encoder -> 'v state
  end
end

type writer = Level0.IOVec.t list -> int
type vec = {off: int option; len: int option}

val stdout : writer
(** Use [Pervasives.output_*] on [stdout] functions to emit bytes. *)

module Make (Level0 : ENCODER) : sig
  type t = { writer : writer
           ; encoder : Level0.encoder }
  val make : writer -> Level0.encoder -> t
  val with_writer : Level0.encoder -> writer -> t

  type 'a encoding = t -> 'a -> t Level0.state
  type 'a sub = t -> ?off:int -> ?len:int -> 'a -> t Level0.state

  val continue : t -> Level0.encoder -> t Level0.state
  val flush : writer -> Level0.encoder -> t Level0.state

  val char : char encoding
  val int8 : int encoding
  val beint16 : int encoding
  val leint16 : int encoding
  val beint32 : int32 encoding
  val leint32 : int32 encoding
  val beint64 : int64 encoding
  val leint64 : int64 encoding
  val substring : string sub
  val subbytes : bytes sub
  val subbigstring : Bigstringaf.t sub
  val string : string encoding
  val bytes : bytes encoding
  val bigstring : Bigstringaf.t encoding
  val whole : 'a sub -> 'a encoding
  val sub : 'a sub -> (vec * 'a) encoding
  val seq : 'a encoding -> 'b encoding -> ('a * 'b) encoding
  val option : 'a encoding -> 'a option encoding
  val list : ?sep:'a encoding * 'a -> 'x encoding -> 'x list encoding
  val using : ('f -> 't) -> 't encoding -> 'f encoding

  type ('ty, 'v) order

  type ('ty, 'v) fmt =
    | [] : ('v, 'v) fmt
    | ( :: ) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

  val atom : 'a encoding -> ('a -> 'v, 'v) order
  val subatom : 'a sub -> (vec -> 'a -> 'v, 'v) order
  val concat : ('x, 'v) fmt -> ('v, 'r) fmt -> ('x, 'r) fmt
  val a : ('a encoding -> 'a -> 'v, 'v) order
  val ( !! ) : 'a encoding -> ('a -> 'v, 'v) order
  val ( !^ ) : 'a sub -> (vec -> 'a -> 'v, 'v) order
  val ( ^^ ) : ('x, 'v) fmt -> ('v, 'r) fmt -> ('x, 'r) fmt
  val yield : ('v, 'v) order
  val register : (int -> Level0.encoder -> unit) -> ('v, 'v) order
  val keval : t -> (t Level0.state -> 'v) -> ('ty, 'v) fmt -> 'ty
  val eval : t -> ('ty, Level0.encoder) fmt -> 'ty
  val const : 'a encoding -> 'a -> ('v, 'v) order
  val ( $ ) : 'a encoding -> 'a -> ('v, 'v) order

  val hov : int -> ('v, 'v) order
  val hv : int -> ('v, 'v) order
  val v : int -> ('v, 'v) order
  val h : ('v, 'v) order
  val box : int -> ('v, 'v) order

  val close : ('v, 'v) order

  val cut : ('v, 'v) order
  val space : ('v, 'v) order
  val force_new_line : ('v, 'v) order
  val new_line : ('v, 'v) order

  module Const : sig
    val char : char -> ('v, 'v) order
    val string : ?off:int -> ?len:int -> string -> ('v, 'v) order
    val bytes : ?off:int -> ?len:int -> bytes -> ('v, 'v) order
    val bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> ('v, 'v) order
  end
end
