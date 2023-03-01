let bind f = function Some x -> f x | None -> None
let map f = function Some x -> Some (f x) | None -> None
let ( >>= ) x f = bind f x
let ( >>| ) x f = map f x
let value ~default = function Some x -> x | None -> default
let some x = Some x
let is_some = function Some _ -> true | None -> false
let get_exn = function Some x -> x | None -> invalid_arg "Option.get_exn"
