open Crowbar

let ( <.> ) f g x = f (g x)

let char_from_alphabet alphabet =
  map [ range (String.length alphabet) ] (String.make 1 <.> String.get alphabet)

let string_from_alphabet alphabet len =
  let rec go acc = function
    | 0 -> concat_gen_list (const "") acc
    | n -> go (char_from_alphabet alphabet :: acc) (pred n)
  in
  go [] len

let alphabet_from_predicate predicate =
  let len =
    let rec go acc = function
      | 0 -> if predicate (Char.unsafe_chr 0) then acc + 1 else acc
      | n ->
          let acc = if predicate (Char.unsafe_chr n) then acc + 1 else acc in
          go acc (n - 1)
    in
    go 0 255
  in
  let res = Bytes.create len in
  let rec go idx = function
    | 0 ->
        if predicate (Char.unsafe_chr 0) then
          Bytes.unsafe_set res idx (Char.unsafe_chr 0)
    | n ->
        if predicate (Char.unsafe_chr n) then
          Bytes.unsafe_set res idx (Char.unsafe_chr n);
        let idx = if predicate (Char.unsafe_chr n) then idx + 1 else idx in
        go idx (n - 1)
  in
  go 0 255;
  Bytes.unsafe_to_string res

let is_dcontent = function
  | '\033' .. '\090' | '\094' .. '\126' -> true
  | _ -> false

let is_atext = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?' | '^'
  | '_' | '`' | '{' | '}' | '|' | '~' ->
      true
  | _ -> false

let is_obs_no_ws_ctl = function
  | '\001' .. '\008' | '\011' | '\012' | '\014' .. '\031' | '\127' -> true
  | _ -> false

let is_dtext = function
  | '\033' .. '\090' | '\094' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let atext = alphabet_from_predicate is_atext
let dtext = alphabet_from_predicate is_dtext

let ldh_str =
  alphabet_from_predicate (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
    | _ -> false)

let let_dig =
  alphabet_from_predicate (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false)

let dcontent = alphabet_from_predicate is_dcontent
