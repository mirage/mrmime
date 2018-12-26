open Angstrom

let is_bcharsnospace = function
  | '\'' | '(' | ')' | '+' | '_' | ','
  | '-' | '.' | '/' | ':' | '=' | '?' -> true
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | '0' .. '9' -> true
  | _ -> false

let is_bchars = function
  | ' ' -> true
  | c -> is_bcharsnospace c

let make_dash_boundary boundary =
  "--" ^ boundary

let make_delimiter boundary =
  "\r\n" ^ (make_dash_boundary boundary)

let make_close_delimiter boundary =
  (make_delimiter boundary) ^ "--"

(* NOTE: this parser terminate at the boundary, however it does not consume it. *)
let discard_all_to_dash_boundary boundary =
  let check_boundary =
    let dash_boundary = "--" ^ boundary in
    let expected_len = String.length dash_boundary in
    Unsafe.peek expected_len
      (fun ba ~off ~len ->
         let raw = Bigstringaf.substring ba ~off ~len in
         String.equal raw dash_boundary) in
  fix @@ fun m ->
  take_while ((<>) '-') *> peek_char >>= function
  | Some '-' ->
    (check_boundary >>= function
      | true -> return true
      | false -> advance 1 *> m)
  | Some _ -> advance 1 *> m (* impossible case? *)
  | None -> return false
