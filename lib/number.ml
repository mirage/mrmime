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
