type point = int
type zone = { a : point; b : point }
type t = zone option
type 'a with_location = { value : 'a; location : t }
type 'a w = 'a with_location = { value : 'a; location : t }

let make a b =
  if a < 0 || b < 0 then invalid_arg "A point must be positive";
  if a > b then invalid_arg "[a] must be lower or equal to [b]";
  Some { a; b }

let some zone = Some zone
let none = None

let union a b =
  match (a, b) with
  | None, None -> None
  | Some _, None -> a
  | None, Some _ -> b
  | Some { a; b }, Some { a = x; b = y } ->
      let a = (min : int -> int -> int) a x in
      let b = (max : int -> int -> int) b y in
      Some { a; b }

let pp ppf = function
  | Some { a; b } -> Format.fprintf ppf "%d:%d" a b
  | None -> Format.pp_print_string ppf "<none>"

let left = function Some { a; _ } -> Some a | None -> None

let left_exn t =
  match left t with Some left -> left | None -> invalid_arg "<dummy location>"

let right = function Some { b; _ } -> Some b | None -> None

let right_exn t =
  match right t with
  | Some right -> right
  | None -> invalid_arg "<dummy location>"

let length = function Some { a; b } -> Some (b - a) | None -> None

let length_exn t =
  match length t with
  | Some length -> length
  | None -> invalid_arg "<dummy location>"

let without_location : 'a with_location -> 'a = fun { value; _ } -> value
let location { location; _ } = location
let with_location ~location v = { value = v; location }
let inj = with_location
let prj = without_location
