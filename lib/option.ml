let bind f = function Some x -> f x | None -> None
let map f = function Some x -> Some (f x) | None -> None
let ( >>= ) x f = bind f x
let ( >>| ) x f = map f x

let value ~default = function
  | Some x -> x
  | None -> default
