module type ENCODER = sig
  include Fe.ENCODER

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
end

module Peano = struct
  type z = Z : z
  type 'l s = S : 'l -> 'l s
end

module Make (Encoder : ENCODER) = struct
  module Fe = Fe.Make(Encoder)

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
    | Fmt : ('ty, 'v) Fe.fmt -> ('ty, 'v) order

  let cut = Cut
  let space = Space
  let force_new_line = Force_new_line
  let new_line = New_line
  let fmt fmt = Fmt fmt

  type ('ty, 'v) fmt =
    | [] : ('v, 'v) fmt
    | (::) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

  let keval_order : type ty v.
    Fe.t -> (ty, v) order -> (Fe.t Encoder.state -> v) -> ty =
    fun t order k -> match order with
      | Fmt fmt -> Fe.keval t k fmt
      | Cut -> k (Encoder.cut (Fe.continue t) t.Fe.encoder)
      | Space -> k (Encoder.space (Fe.continue t) t.Fe.encoder)
      | Force_new_line -> k (Encoder.force_new_line (Fe.continue t) t.Fe.encoder)
      | New_line -> k (Encoder.new_line (Fe.continue t) t.Fe.encoder)

  let rec keval_fmt : type ty v.
    Fe.t -> (Fe.t Encoder.state -> v) -> (ty, v) fmt -> ty =
    fun t k -> function
      | [] -> k (Fe.force_flush t.Fe.writer t.Fe.encoder)
      | x :: r ->
        let rec k' = function
          | Encoder.End t -> keval_fmt t k r
          | Encoder.Continue {continue; encoder} -> k' (continue encoder)
          | Encoder.Flush {continue; iovecs} ->
            let n = t.Fe.writer iovecs in
            k' (continue n) in
        keval_order t x k'

  type ('ty, 'v, 'l) tree =
    | Leaf : ('ty, 'v) fmt -> ('ty, 'v, Peano.z) tree
    | Node : 'k box * ('ty, 'v, 'l) tree -> ('ty, 'v, 'l Peano.s) tree

  let rec keval : type ty v l.
    Fe.t -> (Fe.t Encoder.state -> v) -> (ty, v, l) tree -> ty =
    fun t k -> function
      | Leaf fmt -> keval_fmt t k fmt
      | Node (Box n, tree) ->
        let rec k' = function
          | Encoder.End t -> keval t k tree
          | Encoder.Continue {continue; encoder} -> k' (continue encoder)
          | Encoder.Flush {continue; iovecs} ->
            let n = t.writer iovecs in
            k' (continue n)
        in
        k' (Encoder.box n (Fe.continue t) t.Fe.encoder)
      | _ -> assert false

end

