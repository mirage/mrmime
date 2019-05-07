type 'a t = Pretty.t -> 'a -> Pretty.t

type ('ty, 'v) order

val keval_order : (Pretty.t -> 'v) -> Pretty.t -> ('ty, 'v) order -> 'ty

val break : indent:int -> len:int -> ('v, 'v) order
val fws : ('v, 'v) order
val spaces : int -> ('v, 'v) order
val cut : ('v, 'v) order

val const : 'a t -> 'a -> ('v, 'v) order
val atom : 'a t -> ('a -> 'v, 'v) order
val a : ('a t -> 'a -> 'v, 'v) order

val (!!) : 'a t -> ('a -> 'v, 'v) order
val ($) : 'a t -> 'a -> ('v, 'v) order

val new_line : ('v, 'v) order

val tbox : int -> ('v, 'v) order
val bbox : ('v, 'v) order
val box : ('v, 'v) order
val close : ('v, 'v) order

val using : ('b -> 'a) -> 'a t -> 'b t

val string : string t
val bytes : Bytes.t t
val bigstring : Bigstringaf.t t
val breakable : string t
val char : char t

val list : sep:('x t * 'x) -> 'v t -> 'v list t
val option : 'a t -> 'a option t

type ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | (::) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

val concat : ('a, 'b) fmt -> ('b, 'c) fmt -> ('a, 'c) fmt
val keval : (Pretty.t -> 'v) -> Pretty.t -> ('ty, 'v) fmt -> 'ty
val eval : Pretty.t -> ('ty, Pretty.t) fmt -> 'ty
