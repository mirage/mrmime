module IOVec = Level0.IOVec

type vec = {off: int option; len: int option}
type writer = IOVec.t list -> int

module type ENCODER = sig
  type encoder

  type 'v state = 'v Level0.state =
    | Flush of {continue : int -> 'v state; iovecs : IOVec.t list}
    | Continue of {continue : Level0.encoder -> 'v state; encoder: Level0.encoder}
    | End of 'v

  val flush : (encoder -> 'v state) -> encoder -> 'v state
  val continue : (encoder -> 'v state) -> encoder -> 'v state
  val write_string : ?off:int -> ?len:int -> string -> (encoder -> 'v state) -> encoder -> 'v state
  val write_char : char -> (encoder -> 'v state) -> encoder -> 'v state
  val write_uint8 : int -> (encoder -> 'v state) -> encoder -> 'v state
  val write_bytes : ?off:int -> ?len:int -> bytes -> (encoder -> 'v state) -> encoder -> 'v state
  val write_bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> (encoder -> 'v state) -> encoder -> 'v state
  val schedule_flush : (int -> encoder -> unit) -> encoder -> encoder

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

let std =
  let write = function
    | {IOVec.buffer= Level0.Buffer.String x; off; len} ->
        output_substring stdout x off len ;
        flush stdout ;
        len
    | {IOVec.buffer= Level0.Buffer.Bytes x; off; len} ->
        output_string stdout (Bytes.sub_string x off len) ;
        flush stdout ;
        len
    | {IOVec.buffer= Level0.Buffer.Bigstring x; off; len} ->
        output_string stdout (Bigstringaf.substring x ~off ~len) ;
        flush stdout ;
        len
  in
  List.fold_left (fun a x -> write x + a) 0

module Make (Level0 : ENCODER) = struct
  type t = {writer: writer; encoder: Level0.encoder}

  let continue : t -> Level0.encoder -> t Level0.state =
   fun t e ->
    let rec go = function
      | Level0.Continue {continue; encoder} -> go (continue encoder)
      | Level0.Flush {continue; iovecs} ->
          let n = t.writer iovecs in
          go (continue n)
      | Level0.End encoder -> Level0.End {t with encoder}
    in
    go (Level0.continue (fun e -> End e) e)

  let flush :
      (IOVec.t list -> int) -> Level0.encoder -> t Level0.state =
   fun w e ->
    let rec go = function
      | Level0.Continue {continue; encoder} -> go (continue encoder)
      | Level0.Flush {continue; iovecs} ->
          let n = w iovecs in
          go (continue n)
      | Level0.End encoder -> Level0.End {writer= w; encoder}
    in
    go (Level0.flush (fun e -> End e) e)

  let make writer encoder = {writer; encoder}
  let with_writer encoder writer = make writer encoder

  type 'a encoding = t -> 'a -> t Level0.state
  type 'a sub = t -> ?off:int -> ?len:int -> 'a -> t Level0.state

  let char : char encoding =
   fun t chr -> Level0.write_char chr (continue t) t.encoder

  let int8 : int encoding =
   fun t x -> Level0.write_uint8 x (continue t) t.encoder

  let beint16 : int encoding =
   fun t x -> Level0.BE.write_uint16 x (continue t) t.encoder

  let leint16 : int encoding =
   fun t x -> Level0.LE.write_uint16 x (continue t) t.encoder

  let beint32 : int32 encoding =
   fun t x -> Level0.BE.write_uint32 x (continue t) t.encoder

  let leint32 : int32 encoding =
   fun t x -> Level0.LE.write_uint32 x (continue t) t.encoder

  let beint64 : int64 encoding =
   fun t x -> Level0.BE.write_uint64 x (continue t) t.encoder

  let leint64 : int64 encoding =
   fun t x -> Level0.LE.write_uint64 x (continue t) t.encoder

  let using : ('f -> 't) -> 't encoding -> 'f encoding =
   fun cast encoding t x -> encoding t (cast x)

  let substring : string sub =
   fun t ?off ?len x -> Level0.write_string ?off ?len x (continue t) t.encoder

  let subbytes : bytes sub =
   fun t ?off ?len x -> Level0.write_bytes ?off ?len x (continue t) t.encoder

  let subbigstring : Bigstringaf.t sub =
   fun t ?off ?len x ->
    Level0.write_bigstring ?off ?len x (continue t) t.encoder

  let whole : 'a sub -> 'a encoding = fun a -> a ?off:None ?len:None

  let sub : 'a sub -> (vec * 'a) encoding =
   fun a t ({off; len}, x) -> a ?off ?len t x

  let string = whole substring
  let bytes = whole subbytes
  let bigstring = whole subbigstring

  type ('ty, 'v) order =
    | Const : 'a encoding * 'a -> ('v, 'v) order
    | Atom : 'a encoding -> ('a -> 'v, 'v) order
    | SubAtom : 'a sub -> (vec -> 'a -> 'v, 'v) order
    | Yield : ('v, 'v) order
    | Flush : (int -> Level0.encoder -> unit) -> ('v, 'v) order
    | Param : ('a encoding -> 'a -> 'v, 'v) order

  let keval_order : type ty v.
      t -> (ty, v) order -> (t Level0.state -> v) -> ty =
   fun t order k ->
    match order with
    | Const (encoding, v) -> k (encoding t v)
    | Atom encoding -> fun v -> k (encoding t v)
    | SubAtom encoding -> fun v x -> k (sub encoding t (v, x))
    | Param -> fun encoding v -> k (encoding t v)
    | Flush f ->
        let encoder = Level0.schedule_flush f t.encoder in
        k (continue t encoder)
    | Yield -> k (flush t.writer t.encoder)

  let a = Param
  let const f x = Const (f, x)
  let const_sub a ?off ?len x = Const ((fun t x -> a t ?off ?len x), x)
  let atom f = Atom f
  let subatom f = SubAtom f
  let yield = Yield
  let register f = Flush f

  let seq f g ({writer= w; _} as t) (x, y) =
    let rec go = function
      | Level0.Continue {continue; encoder} -> go (continue encoder)
      | Level0.Flush {continue; iovecs} ->
          let n = w iovecs in
          go (continue n)
      | Level0.End t -> g t y
    in
    go (f t x)

  let noop : 'a encoding = fun t _ -> continue t t.encoder

  let list : ?sep:'a encoding * 'a -> 'b encoding -> 'b list encoding =
   fun ?sep encoding ->
    let sep =
      match sep with None -> noop | Some (a, v) -> fun t () -> a t v
    in
    let rec go t = function
      | [] -> continue t t.encoder
      | [x] -> encoding t x
      | x :: r ->
          let rec go' = function
            | Level0.Continue {continue; encoder} -> go' (continue encoder)
            | Level0.Flush {continue; iovecs} ->
                let n = t.writer iovecs in
                go' (continue n)
            | Level0.End t -> go t
          in
          go' (seq encoding sep t (x, ())) r
    in
    go

  let option encoding t = function Some x -> encoding t x | None -> noop t ()
  let ( !! ) = atom
  let ( !^ ) = subatom
  let ( $ ) = const

  module Const = struct
    let char x = const char x
    let string ?off ?len x = const_sub substring ?off ?len x
    let bytes ?off ?len x = const_sub subbytes ?off ?len x
    let bigstring ?off ?len x = const_sub subbigstring ?off ?len x
  end

  type ('ty, 'v) fmt =
    | [] : ('v, 'v) fmt
    | ( :: ) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

  let rec concat : type x v r. (x, v) fmt -> (v, r) fmt -> (x, r) fmt =
   fun l1 l2 -> match l1 with [] -> l2 | x :: r -> x :: concat r l2

  let ( ^^ ) = concat

  let rec keval : type ty v. t -> (t Level0.state -> v) -> (ty, v) fmt -> ty =
   fun t k fmt ->
    match fmt with
    | [] ->
        k (Level0.End t)
    | x :: r ->
        let k' state =
          let rec go = function
            | Level0.End t -> keval t k r
            | Level0.Continue {continue; encoder} -> go (continue encoder)
            | Level0.Flush {continue; iovecs} ->
                let n = t.writer iovecs in
                go (continue n)
          in
          go state
        in
        keval_order t x k'

  let eval : t -> ('ty, 'v) fmt -> 'ty =
   fun t fmt ->
    let rec finish = function
      | Level0.Continue {continue; encoder} -> finish (continue encoder)
      | Level0.Flush {continue; iovecs} ->
          let n = t.writer iovecs in
          finish (continue n)
      | Level0.End {encoder; _} -> encoder
    in
    keval t finish fmt
end
