type t
type writer = Encoder.IOVec.t list -> int

type vec = {off: int option; len: int option}

val make : writer -> Encoder.encoder -> t
val with_writer : Encoder.encoder -> writer -> t
val std : writer

type 'a encoding = t -> 'a -> t Encoder.state
type 'a sub = t -> ?off:int -> ?len:int -> 'a -> t Encoder.state

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
val list : ?sep:('a encoding * 'a) -> 'x encoding -> 'x list encoding
val using : ('f -> 't) -> 't encoding -> 'f encoding

type ('ty, 'v) order

type ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | (::) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

val atom : 'a encoding -> ('a -> 'v, 'v) order
val subatom : 'a sub -> (vec -> 'a -> 'v, 'v) order
val concat : ('x, 'v) fmt -> ('v, 'r) fmt -> ('x, 'r) fmt
val a : ('a encoding -> 'a -> 'v, 'v) order

val (!!) : 'a encoding -> ('a -> 'v, 'v) order
val (!^) : 'a sub -> (vec -> 'a -> 'v, 'v) order
val (^^) : ('x, 'v) fmt -> ('v, 'r) fmt -> ('x, 'r) fmt

val yield : ('v, 'v) order
val flush : (int -> Encoder.encoder -> unit) -> ('v, 'v) order

val keval : t -> (t Encoder.state -> 'v) -> ('ty, 'v) fmt -> 'ty
val eval : t -> ('ty, Encoder.encoder) fmt -> 'ty

val const : 'a encoding -> 'a -> ('v, 'v) order
val ($) : 'a encoding -> 'a -> ('v, 'v) order

module Const : sig
  val char : char -> ('v, 'v) order
  val string : ?off:int -> ?len:int -> string -> ('v, 'v) order
  val bytes : ?off:int -> ?len:int -> bytes -> ('v, 'v) order
  val bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> ('v, 'v) order
end

module Box : sig
  type 'kind box

  val hov : [ `H | `V ] box
  val hav : [ `HV ] box
  val h : [ `H ] box
  val v : [ `V ] box
  val none : [ `None ] box

  type ('ty, 'v) order

  val cut : ('v, 'v) order
  val space : ('v, 'v) order
  val full : ('v, 'v) order
  val fmt : ('ty, 'v) fmt -> ('ty, 'v) order

  type ('ty, 'v) fmt =
    | [] : ('v, 'v) fmt
    | (::) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

  type z = Z : z
  type 'x s = S : 'x -> 'x s

  type ('ty, 'v, 's) tree

  val node : 'kind box -> ('ty, 'v, 's) tree -> ('ty, 'v, 's s) tree
  val leaf : ('ty, 'v) fmt -> ('ty, 'v, z) tree

  val keval : t -> (t Encoder.state -> 'v) -> ('ty, 'v, 's) tree -> 'ty
end
