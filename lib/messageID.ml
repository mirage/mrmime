type domain = [ `Literal of string | `Domain of string list ]
type t = Emile.local * domain

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let pp_domain : Format.formatter -> domain -> unit =
 fun ppf -> function
  | `Domain _ as x -> Emile.pp_domain ppf x
  | `Literal _ as x -> Emile.pp_domain ppf x

let pp ppf (local, domain) =
  Format.fprintf ppf "<%a@%a>" Emile.pp_local local pp_domain domain

let equal_domain a b =
  match (a, b) with
  | a, b -> Emile.equal_domain (a :> Emile.domain) (b :> Emile.domain)

let equal a b =
  Emile.equal_local ~case_sensitive:true (fst a) (fst b)
  && equal_domain (snd a) (snd b)

module Decoder = struct
  open Angstrom

  let message_id =
    Emile.Parser.msg_id >>= fun (local, domain) ->
    match domain with
    | `Addr _ -> fail "Invalid message-id"
    | #domain as domain -> return (local, domain)
end

let of_string x =
  match
    Angstrom.parse_string ~consume:Angstrom.Consume.Prefix Decoder.message_id x
  with
  | Ok v -> Ok v
  | Error _ -> error_msgf "Invalid message ID: %S" x

module Encoder = struct
  open Prettym

  let dot = ((fun ppf () -> eval ppf [ cut; char $ '.'; cut ]), ())

  let domain : domain Prettym.t =
   fun ppf -> function
    | `Domain domain ->
        let x ppf x = eval ppf [ box; !!string; close ] x in
        eval ppf [ tbox 1; !!(list ~sep:dot x); close ] domain
    | `Literal literal ->
        eval ppf [ tbox 1; char $ '['; !!string; char $ ']'; close ] literal

  let message_id ppf t =
    match t with
    | local_part, domain_part ->
        eval ppf
          [ tbox 1;
            char $ '<';
            !!Mailbox.Encoder.local;
            char $ '@';
            !!domain;
            char $ '>';
            close
          ]
          local_part domain_part
end

let is_utf8_valid_string_with is x =
  let exception Invalid_utf8 in
  let exception Invalid_char in
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function
        | `Malformed _ -> raise Invalid_utf8
        | `Uchar uchar ->
            if Uchar.is_char uchar && not (is (Uchar.to_char uchar)) then
              raise Invalid_char)
      () x;
    true
  with
  | Invalid_utf8 -> false
  | Invalid_char -> false

let is_utf8_valid_string x =
  let exception Invalid_utf8 in
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function `Malformed _ -> raise Invalid_utf8 | _ -> ())
      () x;
    true
  with Invalid_utf8 -> false

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

let is_atext_valid_string = is_utf8_valid_string_with is_atext
let is_dtext_valid_string = is_utf8_valid_string_with is_dtext

module Domain = struct
  let atom x = if is_atext_valid_string x then Some (`Atom x) else None

  let atom_exn x =
    match atom x with
    | Some v -> v
    | None -> invalid_arg "atom_exn: invalid atom value %S" x

  let a = atom_exn

  let literal x =
    let need_to_escape, escape_char =
      (* TODO *)
      let bindings = [ ('\000', '\000') ] in
      ( (fun chr -> List.mem_assoc chr bindings),
        fun chr -> List.assoc chr bindings )
    in
    let escape_string x =
      let len = String.length x in
      let res = Buffer.create (len * 2) in
      let pos = ref 0 in
      while !pos < len do
        if need_to_escape x.[!pos] then (
          Buffer.add_char res '\\';
          Buffer.add_char res (escape_char x.[!pos]))
        else Buffer.add_char res x.[!pos];
        incr pos
      done;
      Buffer.contents res
    in
    if is_dtext_valid_string x then Some (`Literal x)
    else if is_utf8_valid_string x then Some (`Literal (escape_string x))
    else None

  let literal_exn x =
    match literal x with
    | Some v -> v
    | None -> invalid_arg "literal_exn: invalid domain literal value %S" x

  type atom = [ `Atom of string ]
  type literal = [ `Literal of string ]

  type 'a domain =
    | ( :: ) : atom * 'a domain -> 'a Peano.s domain
    | [] : Peano.z domain

  let rec coerce : type a. a Peano.s domain -> string list = function
    | [ `Atom x ] -> [ x ]
    | `Atom x :: y :: r -> List.cons x (coerce (y :: r))

  let make_domain : type a. a domain -> string list option = function
    | [] -> None
    | x :: r -> Some (coerce (x :: r))

  type 'a t = Domain : 'a domain t | Literal : string t

  let domain = Domain
  let default = Literal

  let make :
      type a. a t -> a -> [ `Literal of string | `Domain of string list ] option
      =
   fun witness v ->
    match witness with
    | Domain -> Option.(make_domain v >>| fun v -> `Domain v)
    | Literal -> literal v

  let v : type a. a t -> a -> [ `Literal of string | `Domain of string list ] =
   fun witness v ->
    match make witness v with
    | Some v -> v
    | None -> invalid_arg "make_exn: invalid domain"

  let to_string x = Prettym.to_string Encoder.domain x
end
