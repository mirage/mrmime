type point = int
type zone = { a : point
            ; b : point }
type t = zone option

let make a b =
  if a < 0 || b < 0 then Fmt.invalid_arg "A point must be positive" ;
  Some { a; b; }

let some zone = Some zone
let none = None

let pp ppf = function
  | Some { a; b; } -> Fmt.pf ppf "%d:%d" a b
  | None -> Fmt.string ppf "<none>"
