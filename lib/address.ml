type word = [`Atom of string | `String of string]
type phrase = [`Dot | `Word of word | `Encoded of Encoded_word.t] list

type literal_domain = Rfc5321.literal_domain =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Ext of string * string

type domain =
  [`Domain of string list | `Literal of string | `Addr of literal_domain]

type local = word list

type mailbox = Rfc5322.mailbox =
  {name: phrase option; local: local; domain: domain * domain list}

exception Invalid_utf8
exception Invalid_char

let is_utf8_valid_string_with is x =
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
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function `Malformed _ -> raise Invalid_utf8 | _ -> ())
      () x ;
    true
  with Invalid_utf8 -> false

let is_atext_valid_string = is_utf8_valid_string_with Rfc822.is_atext
let is_dtext_valid_string = is_utf8_valid_string_with Rfc822.is_dtext
let is_qtext_valid_string = is_utf8_valid_string_with Rfc822.is_qtext

let make_word raw =
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
  if is_atext_valid_string raw then Some (`Atom raw)
  else if is_qtext_valid_string raw then Some (`String raw)
  else if is_utf8_valid_string raw then Some (`String (escape_string raw))
  else None

module Peano = struct type z = Z

                      and 'a s = S end

module Phrase = struct
  type elt = [`Word of Rfc822.word | `Encoded of Encoded_word.t | `Dot]
  type 'a t = [] : Peano.z t | ( :: ) : elt * 'a t -> 'a Peano.s t

  let o : elt = `Dot
  let word x : elt option = Option.(make_word x >>| fun x -> `Word x)

  let word_exn x : elt =
    match word x with
    | Some v -> v
    | None -> Fmt.invalid_arg "word_exn: invalid word %S" x

  let w : string -> elt = word_exn
  let e ~encoding v : elt = `Encoded (Encoded_word.make_exn ~encoding v)

  let rec coerce : type a. a Peano.s t -> phrase = function
    | [x] -> [(x :> elt)]
    | x :: y :: r -> List.cons (x :> elt) (coerce (y :: r))

  let make : type a. a t -> phrase option = function
    | [] -> None
    | x :: r -> Some (coerce (x :: r))

  let make_exn l =
    match make l with
    | Some v -> v
    | None -> Fmt.invalid_arg "make_exn: invalid phrase"
end

module Literal_domain = struct
  type 'a t =
    | IPv4 : Ipaddr.V4.t t
    | IPv6 : Ipaddr.V6.t t
    | Ext : (string * string) t

  let is_ldh_valid_string x =
    try
      let len = String.length x in
      String.iteri
        (fun pos -> function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> ()
          | '-' -> if pos = len - 1 then raise Invalid_char (* else () *)
          | _ -> raise Invalid_char )
        x ;
      true
    with Invalid_char -> false

  let is_dcontent_valid_string x =
    try
      String.iter
        (fun chr -> if not (Rfc5321.is_dcontent chr) then raise Invalid_char)
        x ;
      true
    with Invalid_char -> false

  let ipv4 = IPv4
  let ipv6 = IPv6
  let extension = Ext

  let make : type a. a t -> a -> literal_domain option =
   fun witness v ->
    match witness with
    | IPv4 -> Some (Rfc5321.IPv4 v)
    | IPv6 -> Some (Rfc5321.IPv6 v)
    | Ext ->
        let ldh, value = v in
        if is_ldh_valid_string ldh && is_dcontent_valid_string value then
          Some (Rfc5321.Ext (ldh, value))
        else None

  let make_exn : type a. a t -> a -> literal_domain =
   fun witness v ->
    match make witness v with
    | Some v -> v
    | None -> Fmt.invalid_arg "make_exn: invalid literal-domain"
end

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
    | Literal_domain : 'a Literal_domain.t -> 'a t
    | Literal : literal t

  let domain = Domain
  let ipv4 = Literal_domain Literal_domain.IPv4
  let ipv6 = Literal_domain Literal_domain.IPv6
  let extension = Literal_domain Literal_domain.Ext
  let default = Literal

  let make : type a. a t -> a -> Rfc5322.domain option =
   fun witness v ->
    match witness with
    | Domain -> Option.(make_domain v >>| fun v -> `Domain v)
    | Literal_domain witness ->
        Option.(Literal_domain.make witness v >>| fun v -> `Addr v)
    | Literal ->
        let (`Literal v) = v in
        Some (`Literal v)

  let make_exn : type a. a t -> a -> Rfc5322.domain =
   fun witness v ->
    match make witness v with
    | Some v -> v
    | None -> Fmt.invalid_arg "make_exn: invalid domain"
end

module Local = struct
  type 'a local =
    | [] : Peano.z local
    | ( :: ) : word * 'a local -> 'a Peano.s local

  let word x = make_word x

  let word_exn x =
    match word x with
    | Some v -> v
    | None -> Fmt.invalid_arg "word_exn: invalid word value %S" x

  let w = word_exn

  let rec coerce : type a. a Peano.s local -> Rfc822.local = function
    | [x] -> List.cons x []
    | x :: y :: r -> List.cons x (coerce (y :: r))

  let make : type a. a local -> Rfc822.local option = function
    | [] -> None
    | x :: r -> Some (coerce (x :: r))

  let make_exn : type a. a local -> Rfc822.local =
   fun l ->
    match make l with
    | Some v -> v
    | None -> Fmt.invalid_arg "make_exn: invalid local part"
end

let ( @ ) : 'a Local.local -> 'b Domain.t * 'b -> Rfc5322.mailbox option =
 fun local (witness, domain) ->
  match (Local.make local, Domain.make witness domain) with
  | Some local, Some domain ->
      Some {Rfc5322.name= None; local; domain= (domain, [])}
  | _, _ -> None

let with_name : Rfc5322.phrase -> Rfc5322.mailbox -> Rfc5322.mailbox =
 fun name mailbox -> {mailbox with Rfc5322.name= Some name}

let pp_word ppf = function
  | `Atom x -> Fmt.string ppf x
  | `String x -> Fmt.pf ppf "%S" x

let pp_phrase : phrase Fmt.t =
  let pp_elt ppf = function
    | `Dot -> Fmt.string ppf "."
    | `Word w -> pp_word ppf w
    | `Encoded e -> Encoded_word.pp ppf e
  in
  Fmt.list ~sep:Fmt.(fun ppf () -> fmt "@ " ppf) pp_elt

let pp_literal_domain ppf = function
  | IPv4 v -> Ipaddr.V4.pp_hum ppf v
  | IPv6 v -> Ipaddr.V6.pp_hum ppf v
  | Ext (ldh, value) -> Fmt.pf ppf "%s:%s" ldh value

let pp_domain ppf = function
  | `Domain l -> Fmt.list ~sep:Fmt.(const string ".") Fmt.string ppf l
  | `Literal s -> Fmt.pf ppf "[%s]" s
  | `Addr x -> Fmt.pf ppf "[%a]" pp_literal_domain x

let pp_local = Fmt.list ~sep:Fmt.(const string ".") pp_word

let pp_mailbox : mailbox Fmt.t =
 fun ppf x ->
  Fmt.pf ppf "{ @[<hov>name = %a;@ local = %a;@ domain = %a@] }"
    Fmt.(hvbox (Dump.option pp_phrase))
    x.Rfc5322.name
    Fmt.(hvbox pp_local)
    x.Rfc5322.local
    Fmt.(hvbox (Dump.pair pp_domain (Dump.list pp_domain)))
    x.Rfc5322.domain
