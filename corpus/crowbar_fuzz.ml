open Crowbar

type 'a t = 'a gen

type ('a, 'b) list = ('a, 'b) gens =
  | [] : ('res, 'res) list
  | ( :: ) : 'a t * ('k, 'res) list -> ('a -> 'k, 'res) list

let int = int
let pair = pair
let char = char
let float = float
let bool = bool
let range = range
let choose = choose
let const = const
let list = list
let list1 = list1
let option = option
let map = map
let fix = fix

let string = bytes
let concat ~sep = concat_gen_list sep
let fixed = bytes_fixed
let ( >>= ) = dynamic_bind
exception Bad
let bad_test (_str:string) = bad_test ()
