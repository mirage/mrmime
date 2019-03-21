type t = string

let compare a b =
  let a = String.lowercase_ascii a in
  let b = String.lowercase_ascii b in
  String.compare a b

let equal a b = compare a b = 0

let capitalize x =
  let capitalize res idx =
    let map = function 'a' .. 'z' as chr  -> Char.unsafe_chr (Char.code chr - 32) | chr -> chr in
    Bytes.set res idx (map (Bytes.get res idx)) in
  let is_dash_or_space = function ' ' | '-' -> true | _ -> false in
  let res = Bytes.of_string x in
  for i = 0 to String.length x - 1 do
    if i > 0 && is_dash_or_space x.[i - 1]
    then capitalize res i
    else if i = 0 then capitalize res i
  done ; Bytes.unsafe_to_string res

let canonicalize = String.lowercase_ascii

exception Break

let of_string x =
  try
    for i = 0 to String.length x - 1
    do if not (Rfc5322.is_ftext x.[i]) then raise Break done ;
    Ok (canonicalize x)
  with Break -> Rresult.R.error_msgf "Invalid field: %S" x

let of_string_exn x = match of_string x with
  | Ok x -> x
  | Error (`Msg err) -> Fmt.invalid_arg "%s" err

let v = of_string_exn

let pp = Fmt.using capitalize Fmt.string

let prefixed_by prefix field =
  if String.contains prefix '-' then Fmt.invalid_arg "Field.prefixed_by: %s contains '-'" prefix ;
  match String.(split_on_char '-' (lowercase_ascii field)) with
  | [] -> assert false (* XXX(dinosaure): see invariants of [split_on_char]. *)
  | [ x ] -> false
  | x :: r -> String.(equal x (lowercase_ascii prefix))

module Encoder = struct
  open Encoder

  let field = using capitalize string
end
