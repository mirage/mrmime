let bind f = function Some x -> f x | None -> None
let ( >>= ) x f = bind f x
