type t = int

let zero = 0
let succ = succ
let pred = function
  | 0 -> None
  | n -> Some (pred n)
let compare a b = a - b
let equal a b = a == b (* safe *)
let of_int_exn n =
  if n < 0 then Fmt.invalid_arg "Number must be > 0" ; n
let min : t -> t -> t = min
let max : t -> t -> t = max
let add : t -> t -> t = (+)
let add_int : int -> t -> (t, [ `Msg of string ]) result = fun a b ->
  if a < 0 then Rresult.R.error_msgf "%d must be upper or equal to 0" a else Ok (a + b)
