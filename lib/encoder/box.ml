module type ENCODER = sig
  include Format.ENCODER

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
end

module Peano = struct
  type z = Z : z
  type 'l s = S : 'l -> 'l s
end

module Make (Level0 : ENCODER) = struct
  module Format = Format.Make(Level0)

  type 'kind box =
    | HoV : int -> [ `H | `V ] box
    | HaV : int -> [ `HV ] box
    | H : [ `H ] box
    | V : int -> [ `V ] box
    | Box : int -> [ `Box ] box

  let hov n = HoV n
  let hav n = HaV n
  let h = H
  let v n = V n
  let box n = Box n

  type ('ty, 'v) order =
    | Cut : ('v, 'v) order
    | Space : ('v, 'v) order
    | Force_new_line : ('v, 'v) order
    | New_line : ('v, 'v) order
    | Format : ('ty, 'v) Format.fmt -> ('ty, 'v) order

  let cut = Cut
  let space = Space
  let force_new_line = Force_new_line
  let new_line = New_line
  let fmt fmt = Format fmt

  type ('ty, 'v) fmt =
    | [] : ('v, 'v) fmt
    | (::) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

  let keval_order : type ty v.
    Format.t -> (ty, v) order -> (Format.t Level0.state -> v) -> ty =
    fun t order k -> match order with
      | Format fmt -> Format.keval t k fmt
      | Cut -> k (Level0.cut (Format.continue t) t.Format.encoder)
      | Space -> k (Level0.space (Format.continue t) t.Format.encoder)
      | Force_new_line -> k (Level0.force_new_line (Format.continue t) t.Format.encoder)
      | New_line -> k (Level0.new_line (Format.continue t) t.Format.encoder)

  let rec keval_fmt : type ty v.
    Format.t -> (Format.t Level0.state -> v) -> (ty, v) fmt -> ty =
    fun t k -> function
      | [] -> k (Level0.End t)
      | x :: r ->
        let rec k' = function
          | Level0.End t -> keval_fmt t k r
          | Level0.Continue {continue; encoder} -> k' (continue encoder)
          | Level0.Flush {continue; iovecs} ->
            let n = t.Format.writer iovecs in
            k' (continue n) in
        keval_order t x k'

  type ('ty, 'v, 'l) tree =
    | Leaf : ('ty, 'v) fmt -> ('ty, 'v, Peano.z) tree
    | Node : 'k box * ('ty, 'v, 'l) tree -> ('ty, 'v, 'l Peano.s) tree

  let rec keval : type ty v l.
    Format.t -> (Format.t Level0.state -> v) -> (ty, v, l) tree -> ty =
    fun t k x ->
      let rec k_close = function
        | Level0.End t -> k (Level0.close_box (Format.continue t) t.Format.encoder)
        | Level0.Continue {continue; encoder} -> k_close (continue encoder)
        | Level0.Flush {continue; iovecs} ->
          let n = t.writer iovecs in
          k_close (continue n) in
      let rec k_open tree = function
        | Level0.End t -> keval t k_close tree
        | Level0.Continue {continue; encoder} -> k_open tree (continue encoder)
        | Level0.Flush {continue; iovecs} ->
          let n = t.writer iovecs in
          k_open tree (continue n) in
      match x with
      | Leaf fmt -> keval_fmt t k fmt
      | Node (Box n, tree) ->
        k_open tree (Level0.box n (Format.continue t) t.Format.encoder)
      | Node (HoV n, tree) ->
        k_open tree (Level0.hovbox n (Format.continue t) t.Format.encoder)
      | Node (HaV n, tree) ->
        k_open tree (Level0.hvbox n (Format.continue t) t.Format.encoder)
      | Node (H, tree) ->
        k_open tree (Level0.hbox (Format.continue t) t.Format.encoder)
      | Node (V n, tree) ->
        k_open tree (Level0.vbox n (Format.continue t) t.Format.encoder)

  let eval : Format.t -> ('ty, 'v, 'l) tree -> 'ty =
    fun t tree ->
      let rec finish = function
        | Level0.Continue {continue; encoder} -> finish (continue encoder)
        | Level0.Flush {continue; iovecs} ->
          let n = t.Format.writer iovecs in
          finish (continue n)
        | Level0.End { Format.encoder; _ } -> encoder in
      keval t finish tree

  let o fmt = Leaf fmt
  let node k t = Node (k, t)
end

