module IOVec = Encoder.IOVec

type vec = {off: int option; len: int option}
type writer = IOVec.t list -> int

module type ENCODER = sig
  type encoder

  type 'v state = 'v Encoder.state =
    | Flush of {continue : int -> 'v state; iovecs : IOVec.t list}
    | Continue of {continue : Encoder.encoder -> 'v state; encoder: Encoder.encoder}
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
    | {IOVec.buffer= Encoder.Buffer.String x; off; len} ->
        output_substring stdout x off len ;
        flush stdout ;
        len
    | {IOVec.buffer= Encoder.Buffer.Bytes x; off; len} ->
        output_string stdout (Bytes.sub_string x off len) ;
        flush stdout ;
        len
    | {IOVec.buffer= Encoder.Buffer.Bigstring x; off; len} ->
        output_string stdout (Bigstringaf.substring x ~off ~len) ;
        flush stdout ;
        len
  in
  List.fold_left (fun a x -> write x + a) 0

module Make (Encoder : ENCODER) = struct
  type t = {writer: writer; encoder: Encoder.encoder}

  let continue : t -> Encoder.encoder -> t Encoder.state =
   fun t e ->
    let rec go = function
      | Encoder.Continue {continue; encoder} -> go (continue encoder)
      | Encoder.Flush {continue; iovecs} ->
          let n = t.writer iovecs in
          go (continue n)
      | Encoder.End encoder -> Encoder.End {t with encoder}
    in
    go (Encoder.continue (fun e -> End e) e)

  let flush :
      (IOVec.t list -> int) -> Encoder.encoder -> t Encoder.state =
   fun w e ->
    let rec go = function
      | Encoder.Continue {continue; encoder} -> go (continue encoder)
      | Encoder.Flush {continue; iovecs} ->
          let n = w iovecs in
          go (continue n)
      | Encoder.End encoder -> Encoder.End {writer= w; encoder}
    in
    go (Encoder.flush (fun e -> End e) e)

  let force_flush = flush

  let make writer encoder = {writer; encoder}
  let with_writer encoder writer = make writer encoder

  type 'a encoding = t -> 'a -> t Encoder.state
  type 'a sub = t -> ?off:int -> ?len:int -> 'a -> t Encoder.state

  let char : char encoding =
   fun t chr -> Encoder.write_char chr (continue t) t.encoder

  let int8 : int encoding =
   fun t x -> Encoder.write_uint8 x (continue t) t.encoder

  let beint16 : int encoding =
   fun t x -> Encoder.BE.write_uint16 x (continue t) t.encoder

  let leint16 : int encoding =
   fun t x -> Encoder.LE.write_uint16 x (continue t) t.encoder

  let beint32 : int32 encoding =
   fun t x -> Encoder.BE.write_uint32 x (continue t) t.encoder

  let leint32 : int32 encoding =
   fun t x -> Encoder.LE.write_uint32 x (continue t) t.encoder

  let beint64 : int64 encoding =
   fun t x -> Encoder.BE.write_uint64 x (continue t) t.encoder

  let leint64 : int64 encoding =
   fun t x -> Encoder.LE.write_uint64 x (continue t) t.encoder

  let using : ('f -> 't) -> 't encoding -> 'f encoding =
   fun cast encoding t x -> encoding t (cast x)

  let substring : string sub =
   fun t ?off ?len x -> Encoder.write_string ?off ?len x (continue t) t.encoder

  let subbytes : bytes sub =
   fun t ?off ?len x -> Encoder.write_bytes ?off ?len x (continue t) t.encoder

  let subbigstring : Bigstringaf.t sub =
   fun t ?off ?len x ->
    Encoder.write_bigstring ?off ?len x (continue t) t.encoder

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
    | Flush : (int -> Encoder.encoder -> unit) -> ('v, 'v) order
    | Param : ('a encoding -> 'a -> 'v, 'v) order

  let keval_order : type ty v.
      t -> (ty, v) order -> (t Encoder.state -> v) -> ty =
   fun t order k ->
    match order with
    | Const (encoding, v) -> k (encoding t v)
    | Atom encoding -> fun v -> k (encoding t v)
    | SubAtom encoding -> fun v x -> k (sub encoding t (v, x))
    | Param -> fun encoding v -> k (encoding t v)
    | Flush f ->
        let encoder = Encoder.schedule_flush f t.encoder in
        k (continue t encoder)
    | Yield -> k (flush t.writer t.encoder)

  let a = Param
  let const f x = Const (f, x)
  let const_sub a ?off ?len x = Const ((fun t x -> a t ?off ?len x), x)
  let atom f = Atom f
  let subatom f = SubAtom f
  let yield = Yield
  let flush f = Flush f

  let seq f g ({writer= w; _} as t) (x, y) =
    let rec go = function
      | Encoder.Continue {continue; encoder} -> go (continue encoder)
      | Encoder.Flush {continue; iovecs} ->
          let n = w iovecs in
          go (continue n)
      | Encoder.End t -> g t y
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
            | Encoder.Continue {continue; encoder} -> go' (continue encoder)
            | Encoder.Flush {continue; iovecs} ->
                let n = t.writer iovecs in
                go' (continue n)
            | Encoder.End t -> go t
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

  let rec keval : type ty v. t -> (t Encoder.state -> v) -> (ty, v) fmt -> ty =
   fun t k fmt ->
    match fmt with
    | [] ->
        k
          (Encoder.flush
             (fun e -> Encoder.End {writer= t.writer; encoder= e})
             t.encoder)
    | x :: r ->
        let k' state =
          let rec go = function
            | Encoder.End t -> keval t k r
            | Encoder.Continue {continue; encoder} -> go (continue encoder)
            | Encoder.Flush {continue; iovecs} ->
                let n = t.writer iovecs in
                go (continue n)
          in
          go state
        in
        keval_order t x k'

  let eval : t -> ('ty, 'v) fmt -> 'ty =
   fun t fmt ->
    let rec finish = function
      | Encoder.Continue {continue; encoder} -> finish (continue encoder)
      | Encoder.Flush {continue; iovecs} ->
          let n = t.writer iovecs in
          finish (continue n)
      | Encoder.End {encoder; _} -> encoder
    in
    keval t finish fmt

  module Box = struct
    type 'kind box =
      | HoV : [`H | `V] box
      | HaV : [`HV] box
      | H : [`H] box
      | V : [`V] box
      | None : [`None] box

    let hov = HoV
    let hav = HaV
    let h = H
    let v = V
    let none = None

    type ('ty, 'v) order =
      | Cut : ('v, 'v) order
      | Space : ('v, 'v) order
      | Full : ('v, 'v) order
      | Fmt : ('ty, 'v) fmt -> ('ty, 'v) order

    let cut = Cut
    let space = Space
    let full = Full
    let fmt fmt = Fmt fmt

    type ('ty, 'v) fmt =
      | [] : ('v, 'v) fmt
      | ( :: ) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

    let keval_order : type ty v.
        t -> (ty, v) order -> (t Encoder.state -> v) -> ty =
     fun t order k ->
      match order with
      | Fmt fmt -> keval t k fmt
      | _ -> assert false

    let rec keval_fmt : type ty v.
        t -> (t Encoder.state -> v) -> (ty, v) fmt -> ty =
     fun t k fmts ->
      match fmts with
      | [] ->
          k
            (Encoder.flush
               (fun e -> Encoder.End {writer= t.writer; encoder= e})
               t.encoder)
      | x :: r ->
          let rec k' = function
            | Encoder.End t -> keval_fmt t k r
            | Encoder.Continue {continue; encoder} -> k' (continue encoder)
            | Encoder.Flush {continue; iovecs} ->
                let n = t.writer iovecs in
                k' (continue n)
          in
          keval_order t x k'

    type z = Z : z
    type 'x s = S : 'x -> 'x s

    type ('ty, 'v, 's) tree =
      | Leaf : ('ty, 'v) fmt -> ('ty, 'v, z) tree
      | Node : 'k box * ('ty, 'v, 's) tree -> ('ty, 'v, 's s) tree

    let rec keval : type ty v s.
        t -> (t Encoder.state -> v) -> (ty, v, s) tree -> ty =
     fun t k tree ->
      match tree with
      | Leaf fmt -> keval_fmt t k fmt
      | Node (_, tree) ->
          let rec k' = function
            | Encoder.End t -> keval t k tree
            | Encoder.Continue {continue; encoder} -> k' (continue encoder)
            | Encoder.Flush {continue; iovecs} ->
                let n = t.writer iovecs in
                k' (continue n)
          in
          k' (Encoder.End t)

    (* XXX(dinosaure): apply box. *)

    let node box tree = Node (box, tree)
    let leaf fmt = Leaf fmt
  end
end
