type 'a t =
  | Char : char t
  | Float : float t
  | String : string t
  | Bool : bool t
  | Int : int t
  | Range : { min : int; max : int } -> int t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Choose : 'a t list -> 'a t
  | Const : 'a -> 'a t
  | Concat : { sep : string t; lst : string t list } -> string t
  | List : 'a t -> 'a list t
  | List1 : 'a t -> 'a list t
  | Fixed : int -> string t
  | Option : 'a t -> 'a option t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Fix : 'a t Lazy.t -> 'a t
  | Map : ('f, 'a) gens * 'f -> 'a t

and (_, _) gens =
  | [] : ('res, 'res) gens
  | ( :: ) : 'a t * ('k, 'res) gens -> ('a -> 'k, 'res) gens

let range ?(min = 0) max = Range { min; max }
let list v = List v

exception Bad

let bad_test _where = raise Bad

let rec safe : type a b. g:Mirage_crypto_rng.Fortuna.g -> a t -> (a -> b) -> b =
 fun ~g t f ->
  try
    let v = run ~g t in
    f v
  with Bad -> safe ~g t f

and concat bytes : _ list -> _ = function
  | [] -> ""
  | hd :: _ as lst ->
      let res = Bytes.create bytes in
      let rec go pos : _ list -> _ = function
        | [] -> Bytes.unsafe_to_string res
        | [ x ] ->
            Bytes.blit_string x 0 res 0 (String.length x);
            Bytes.unsafe_to_string res
        | x :: (y :: _ as r) ->
            Bytes.blit_string x 0 res pos (String.length x);
            go (pos - String.length y) r
      in
      go (bytes - String.length hd) lst

and run : type a. g:Mirage_crypto_rng.Fortuna.g -> a t -> a =
 fun ~g -> function
  | Pair (a, b) ->
      let va = run ~g a in
      let vb = run ~g b in
      (va, vb)
  | Char ->
      let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
      Cstruct.get_char cs 0
  | Float ->
      let cs = Mirage_crypto_rng.Fortuna.generate ~g 8 in
      let a = Int32.abs (Cstruct.LE.get_uint32 cs 0) in
      let b = Int32.abs (Cstruct.LE.get_uint32 cs 4) in
      Float.of_string (Fmt.str "%ld.%ld" a b)
  | String ->
      let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
      let ln = Cstruct.get_uint8 cs 0 in
      let cs = Mirage_crypto_rng.Fortuna.generate ~g ln in
      Cstruct.to_string cs
  | Bool ->
      let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
      Cstruct.get_uint8 cs 0 land 1 = 1
  | Int -> (
      match Sys.word_size with
      | 32 ->
          let cs = Mirage_crypto_rng.Fortuna.generate ~g 4 in
          Int32.to_int (Cstruct.LE.get_uint32 cs 0)
      | 64 ->
          let cs = Mirage_crypto_rng.Fortuna.generate ~g 8 in
          Int64.to_int (Cstruct.LE.get_uint64 cs 0)
      | _ -> assert false)
  | Range { min; max = m } ->
      if m < 0x100 then
        let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
        min + (Cstruct.get_uint8 cs 0 mod m)
      else if m < 0x1000000 then
        let cs = Mirage_crypto_rng.Fortuna.generate ~g 4 in
        min + Int32.(to_int (abs (rem (Cstruct.LE.get_uint32 cs 0) (of_int m))))
      else
        let cs = Mirage_crypto_rng.Fortuna.generate ~g 8 in
        min + Int64.(to_int (abs (rem (Cstruct.LE.get_uint64 cs 0) (of_int m))))
  | Bind (x, f) ->
      let v = run ~g x in
      run ~g (f v)
  | Choose lst ->
      let max = List.length lst in
      let nth = range ~min:0 max in
      let nth = run ~g nth in
      run ~g (List.nth lst nth)
  | Const v -> v
  | Concat { sep; lst } -> (
      match lst with
      | [] -> ""
      | _ ->
          let sep = run ~g sep in
          let rec go bytes acc : _ List.t -> _ = function
            | [] -> concat bytes acc
            | x :: r ->
                let x = run ~g x in
                go
                  (String.length x + String.length sep + bytes)
                  (x :: sep :: acc) r
          in
          go 0 [] lst)
  | List t ->
      let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
      let nm = Cstruct.get_uint8 cs 0 land 0xf in
      List.init nm (fun _ -> run ~g t)
  | List1 t ->
      let hd = run ~g t in
      let tl = run ~g (list t) in
      hd :: tl
  | Fixed len ->
      let cs = Mirage_crypto_rng.Fortuna.generate ~g len in
      Cstruct.to_string cs
  | Fix v -> run ~g (Lazy.force v)
  | Map (ts, f) ->
      let rec go : type f a. (f, a) gens -> f -> a = function
        | [] -> fun x -> x
        | x :: r ->
            fun f ->
              let v = safe ~g x f in
              go r v
      in
      go ts f
  | Option t -> (
      let cs = Mirage_crypto_rng.Fortuna.generate ~g 1 in
      match Cstruct.get_uint8 cs 0 land 1 = 1 with
      | true ->
          let v = run ~g t in
          Some v
      | false ->
          let _ = run ~g t in
          None)

let char = Char
let float = Float
let string = String
let bool = Bool
let int = Int
let bind x f = Bind (x, f)
let choose lst = Choose lst
let const v = Const v
let list1 v = List1 v
let fixed len = Fixed len
let option v = Option v

let fix f =
  let rec m = lazy (f (Fix m)) in
  Fix m

let pair a b = Pair (a, b)
let ( >>= ) x f = Bind (x, f)
let map lst f = Map (lst, f)
let concat ~sep lst = Concat { sep; lst }
