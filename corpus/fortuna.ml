type 'a t = Mirage_crypto_rng.Fortuna.g -> 'a

let char g =
  let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
  Cstruct.get cs 0

let float g =
  let cs = Mirage_crypto_rng.Fortuna.generate ~g 8 in
  let v = Cstruct.LE.get_uint64 cs 0 in
  Int64.float_of_bits v

let string g =
  let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
  let ln = Cstruct.get_uint8 cs 0 in
  let cs = Mirage_crypto_rng.Fortuna.generate ~g ln in
  Cstruct.to_string cs

let bool g =
  let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
  Cstruct.get_uint8 cs 0 land 1 = 1

let int g =
  match Sys.word_size with
  | 32 ->
    let cs = Mirage_crypto_rng.Fortuna.generate ~g 4 in
    Int32.to_int (Cstruct.LE.get_uint32 cs 0)
  | 64 ->
    let cs = Mirage_crypto_rng.Fortuna.generate ~g 8 in
    Int64.to_int (Cstruct.LE.get_uint64 cs 0)
  | _ -> assert false

let range ?(min= 0) max g =
  if max < 0x100
  then
    let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
    min + ((Cstruct.get_uint8 cs 0) mod max)
  else if max < 0x1000000
  then
    let cs = Mirage_crypto_rng.Fortuna.generate ~g 4 in
    min + Int32.(to_int (abs (rem (Cstruct.LE.get_uint32 cs 0) (of_int max))))
  else
    let cs = Mirage_crypto_rng.Fortuna.generate ~g 8 in
    min + Int64.(to_int (abs (rem (Cstruct.LE.get_uint64 cs 0) (of_int max))))

let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  = fun x f g -> let v = x g in f v g

let choose lst =
  let max = List.length lst in
  fun g ->
    let nth = range ~min:0 max g in
    (List.nth lst nth) g

let const x _ = x

let concat bytes = function
  | [] -> ""
  | hd :: _ as lst ->
    let res = Bytes.create bytes in
    let rec go pos = function
      | [] -> Bytes.unsafe_to_string res
      | [ x ] ->
        Bytes.blit_string x 0 res 0 (String.length x) ;
        Bytes.unsafe_to_string res
      | x :: (y :: _ as r) ->
        Bytes.blit_string x 0 res pos (String.length x) ;
        go (pos - String.length y) r in
    go (bytes - String.length hd) lst

let concat ~sep lst g =
  let rec go bytes acc = function
    | [] -> concat bytes acc
    | [ x ] -> let str = x g in concat (String.length str) [ str ]
    | x :: r ->
      let x = x g in
      let sep = sep g in
      go (String.length x + String.length sep + bytes) (x :: sep :: acc) r in
  go 0 [] lst

let list t g =
  let rec go acc =
    let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
    match Cstruct.get_uint8 cs 0 land 1 = 1 with
    | true  -> go (t g :: acc)
    | false -> acc in
  go []

let list1 t g =
  let hd = t g in
  let tl = list t g in
  hd :: tl

let fixed len g =
  let cs = Mirage_crypto_rng.Fortuna.generate ~g len in
  Cstruct.to_string cs

let option t g =
  let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
  match Cstruct.get_uint8 cs 0 land 1 = 1 with
  | true  -> let v = t g in Some v
  | false -> let _ = t g in None

let pair ta tb g =
  let va = ta g in
  let vb = tb g in
  (va, vb)

type (_, _) list =
  | [] : ('res, 'res) list
  | ( :: ) : 'a t * ('k, 'res) list -> ('a -> 'k, 'res) list

exception Bad

let bad_test () = raise Bad

let rec safe t f g =
  try let v = t g in f v with Bad -> safe t f g

let map : type f a. (f, a) list -> f -> a t =
  fun ts f g ->
    let rec go : type f a. (f, a) list -> f -> a = function
      | [] -> fun x -> x
      | x :: r -> fun f ->
        let v = safe x f g in
        go r v in
    go ts f
