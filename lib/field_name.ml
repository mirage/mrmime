type t = string

let compare a b =
  let a = String.lowercase_ascii a in
  let b = String.lowercase_ascii b in
  String.compare a b

let equal a b = compare a b = 0
let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let capitalize x =
  let capitalize res idx =
    let map = function
      | 'a' .. 'z' as chr -> Char.unsafe_chr (Char.code chr - 32)
      | chr -> chr
    in
    Bytes.set res idx (map (Bytes.get res idx))
  in
  let is_dash_or_space = function ' ' | '-' -> true | _ -> false in
  let res = Bytes.of_string x in
  for i = 0 to String.length x - 1 do
    if i > 0 && is_dash_or_space x.[i - 1] then capitalize res i
    else if i = 0 then capitalize res i
  done;
  Bytes.unsafe_to_string res

let canonicalize = String.lowercase_ascii

exception Break

let is_ftext = function
  | '\033' .. '\057' | '\059' .. '\126' -> true
  | _ -> false

let of_string x =
  if x = "" then error_msgf "Invalid empty field-name"
  else
    try
      for i = 0 to String.length x - 1 do
        if not (is_ftext x.[i]) then raise Break
      done;
      Ok x
    with Break -> error_msgf "Invalid field: %S" x

let of_string_exn x =
  match of_string x with Ok x -> x | Error (`Msg err) -> invalid_arg err

let v = of_string_exn
let pp ppf x = Format.pp_print_string ppf (capitalize x)
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let prefixed_by prefix field =
  if String.contains prefix '-' then
    invalid_arg "Field.prefixed_by: %s contains '-'" prefix;
  match String.(split_on_char '-' (lowercase_ascii field)) with
  | [] -> assert false (* XXX(dinosaure): see invariants of [split_on_char]. *)
  | [ _ ] -> false
  | x :: _ -> String.(equal x (lowercase_ascii prefix))

module Decoder = struct
  open Angstrom

  let field_name = take_while1 is_ftext
end

module Encoder = struct
  open Prettym

  let field_name = using capitalize string
end

let date = v "Date"
let from = v "From"
let sender = v "Sender"
let reply_to = v "Reply-To"
let cc = v "Cc"
let bcc = v "Bcc"
let subject = v "Subject"
let message_id = v "Message-ID"
let in_reply_to = v "In-Reply-To"
let references = v "References"
let comments = v "Comments"
let keywords = v "Keywords"
let received = v "Received"
let return_path = v "Return-Path"
let content_type = v "Content-Type"
let content_encoding = v "Content-Transfer-Encoding"
let mime_version = v "MIME-Version"
let content_id = v "Content-ID"
let content_description = v "Content-Description"
let resent_date = v "Resent-Date"
let resent_from = v "Resent-From"
let resent_sender = v "Resent-Sender"
let resent_to = v "Resent-To"
let resent_cc = v "Resent-Cc"
let resent_bcc = v "Resent-Bcc"
let resent_message_id = v "Resent-Message-ID"
let resent_reply_to = v "Resent-Reply-To"

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
