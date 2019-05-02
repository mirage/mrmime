type word = [`Atom of string | `String of string]
type local = word list
type domain = Rfc822.nonsense Rfc822.domain
type t = Rfc822.nonsense Rfc822.msg_id (* = local * domain *)

let pp_word ppf = function
  | `Atom x -> Fmt.string ppf x
  | `String x -> Fmt.pf ppf "%S" x

let pp_domain : Rfc822.nonsense Rfc822.domain Fmt.t = fun ppf -> function
  | `Domain l -> Fmt.list ~sep:Fmt.(const string ".") Fmt.string ppf l
  | `Literal x -> Fmt.pf ppf "[%s]" x
  | `Addr _ -> .

let pp_local : local Fmt.t = Fmt.list ~sep:Fmt.(const string ".") pp_word

let pp ppf (local, domain) =
  Fmt.pf ppf "@[<hov>%a@%a@]"
    pp_local local
    pp_domain domain

let equal_word a b = match a, b with
  | `Atom x, `Atom y -> String.equal x y
  | `String x, `String y -> String.equal x y
  | _, _ -> false

let equal_local a b =
  try List.for_all2 equal_word a b with _ -> false

let equal_domain a b = match a, b with
  | `Literal a, `Literal b -> String.equal a b
  | `Domain a, `Domain b -> (try List.for_all2 String.equal a b with _ -> false)
  | _, _ -> false

let equal a b =
  equal_local (fst a) (fst b)
  && equal_domain (snd a) (snd b)

module Encoder = struct
  include Encoder

  external id : 'a -> 'a = "%identity"

  let dot = using (fun () -> '.') char, ()

  let domain : Rfc822.nonsense Rfc822.domain Encoder.encoding = fun ppf -> function
    | `Domain domain ->
      let x ppf x = keval ppf id [ hov 0; !!string; close ] x in
      keval ppf id [ hov 1; !!(list ~sep:dot x); close ] domain
    | `Literal literal ->
      keval ppf id [ hov 1; char $ '['; !!string; char $ ']'; close ] literal
    | `Addr _ -> .

  let message_id ppf (t:Rfc822.nonsense Rfc822.msg_id) =
    match t with
    | (local_part, domain_part) ->
      keval ppf id [ hov 1; char $ '<'; !!Mailbox.Encoder.local; char $ '@'; !!domain; char $ '>'; close ]
        local_part domain_part
end

let to_unstructured ~field_name x = Unstructured.to_unstructured ~field_name Encoder.message_id x

let is_utf8_valid_string_with is x =
  let exception Invalid_utf8 in
  let exception Invalid_char in
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function `Malformed _ -> raise Invalid_utf8
        | `Uchar uchar ->
            if Uchar.is_char uchar && not (is (Uchar.to_char uchar)) then
              raise Invalid_char )
      () x ;
    true
  with
  | Invalid_utf8 -> false
  | Invalid_char -> false

let is_utf8_valid_string x =
  let exception Invalid_utf8 in
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function `Malformed _ -> raise Invalid_utf8 | _ -> ())
      () x ;
    true
  with Invalid_utf8 -> false

let is_atext_valid_string = is_utf8_valid_string_with Rfc822.is_atext
let is_dtext_valid_string = is_utf8_valid_string_with Rfc822.is_dtext
let is_qtext_valid_string = is_utf8_valid_string_with Rfc822.is_qtext

module Domain = struct
  let atom x = if is_atext_valid_string x then Some (`Atom x) else None

  let atom_exn x =
    match atom x with
    | Some v -> v
    | None -> Fmt.invalid_arg "atom_exn: invalid atom value %S" x

  let a = atom_exn

  let literal x =
    let need_to_escape, escape_char =
      (* TODO *)
      let bindings = [('\000', '\000')] in
      ( (fun chr -> List.mem_assoc chr bindings)
      , fun chr -> List.assoc chr bindings )
    in
    let escape_string x =
      let len = String.length x in
      let res = Buffer.create (len * 2) in
      let pos = ref 0 in
      while !pos < len do
        if need_to_escape x.[!pos] then (
          Buffer.add_char res '\\' ;
          Buffer.add_char res (escape_char x.[!pos]) )
        else Buffer.add_char res x.[!pos] ;
        incr pos
      done ;
      Buffer.contents res
    in
    if is_dtext_valid_string x then Some (`Literal x)
    else if is_utf8_valid_string x then Some (`Literal (escape_string x))
    else None

  let literal_exn x =
    match literal x with
    | Some v -> v
    | None -> Fmt.invalid_arg "literal_exn: invalid domain literal value %S" x

  type atom = [`Atom of string]
  type literal = [`Literal of string]

  type 'a domain =
    | ( :: ) : atom * 'a domain -> 'a Peano.s domain
    | [] : Peano.z domain

  let rec coerce : type a. a Peano.s domain -> string list = function
    | [`Atom x] -> [x]
    | `Atom x :: y :: r -> List.cons x (coerce (y :: r))

  let make_domain : type a. a domain -> string list option = function
    | [] -> None
    | x :: r -> Some (coerce (x :: r))

  type 'a t =
    | Domain : 'a domain t
    | Literal : string t

  let domain = Domain
  let default = Literal

  let make : type a. a t -> a -> Rfc822.nonsense Rfc822.domain option =
   fun witness v ->
    match witness with
    | Domain -> Option.(make_domain v >>| fun v -> `Domain v)
    | Literal -> literal v

  let v : type a. a t -> a -> Rfc822.nonsense Rfc822.domain =
   fun witness v ->
    match make witness v with
    | Some v -> v
    | None -> Fmt.invalid_arg "make_exn: invalid domain"

  let to_string x = Encoder.to_string Encoder.domain x
end
