type word = [`Atom of string | `String of string]
type phrase = [`Dot | `Word of word | `Encoded of Encoded_word.t] list

type literal_domain = Rfc5321.literal_domain =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Ext of string * string

type domain =
  [`Domain of string list | `Literal of string | `Addr of literal_domain]

type local = word list

type t = Rfc5322.mailbox =
  {name: phrase option; local: local; domain: domain * domain list}

let equal_word ?(sensitive = false) a b = match a, b with
  | (`Atom a | `String a), (`Atom b | `String b) ->
    if sensitive
    then String.equal a b
    else String.(equal (lowercase_ascii a) (lowercase_ascii b))

let equal_phrase a b =
  let elt a b = match a, b with
    | `Dot, `Dot -> true
    | `Word a, `Word b -> equal_word ~sensitive:true a b
    | `Encoded a, `Encoded b -> Encoded_word.equal a b
    | _, _ -> false (* TODO: [`Encoded a] & [`Word b] or [`Word a] & [`Encoded b]. *) in
  try List.for_all2 elt a b with _ -> false

let equal_literal_domain a b = match a, b with
  | IPv4 a, IPv4 b -> Ipaddr.V4.compare a b = 0
  | IPv6 a, IPv6 b -> Ipaddr.V6.compare a b = 0
  | IPv4 a, IPv6 b -> Ipaddr.(compare (V4 a) (V6 b)) = 0
  | IPv6 a, IPv4 b -> Ipaddr.(compare (V6 a) (V4 b)) = 0
  | Ext (ldh0, value0), Ext (ldh1, value1) ->
    String.equal ldh0 ldh1 && String.equal value0 value1
  | _, _ -> false

let equal_domain a b = match a, b with
  | `Domain a, `Domain b ->
    let a = List.map String.lowercase_ascii a in
    let b = List.map String.lowercase_ascii b in
    (try List.for_all2 String.equal a b with _ -> false)
  | `Literal a, `Literal b ->
    let a = String.lowercase_ascii a in
    let b = String.lowercase_ascii b in
    String.equal a b
  | `Addr a, `Addr b -> equal_literal_domain a b
  | _, _ -> false (* TODO: resolution? *)

let equal_domains a b = try List.for_all2 equal_domain a b with _ -> false

let equal_local a b =
  try List.for_all2 (equal_word ~sensitive:false) a b
  with _ -> false

let equal a b = match a, b with
  | { name= Some name0; local= local0; domain= head0, tail0 },
    { name= Some name1; local= local1; domain= head1, tail1 } ->
    equal_phrase name0 name1 && equal_local local0 local1 && equal_domains (head0 :: tail0) (head1 :: tail1)
  | { name= None; local= local0; domain= head0, tail0 },
    { name= None; local= local1; domain= head1, tail1 } ->
    equal_local local0 local1 && equal_domains (head0 :: tail0) (head1 :: tail1)
  | _, _ -> false

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

let need_to_escape, escape_char =
  (* See [Rfc822.of_escaped_character] but totally arbitrary. *)
  let bindings = [('\000', '\000')
                  ;('\\', '\\')
                  ;('\x07', 'a')
                  ;('\b', 'b')
                  ;('\t', 't')
                  ;('\n', 'n')
                  ;('\x0b', 'v')
                  ;('\x0c', 'f')
                  ;('\r', 'r')
                  ;('"', '"')] in
  ( (fun chr -> List.mem_assoc chr bindings)
  , fun chr -> List.assoc chr bindings )

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

let make_word raw =
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

  let word x = if String.length x > 0 then make_word x else None

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

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let atom = node (hov 0) (o [ fmt Format.[ !!string ] ])
  let str = node (hov 0) (o [ fmt Format.[ char $ '"'; !!string; char $ '"' ] ])

  let word ppf = function
    | `Atom x -> keval ppf id atom x
    | `String x ->
      keval ppf id str (escape_string x)

  let dot = Format.using (fun () -> '.') Format.char, ()

  let local ppf lst =
    keval ppf id (node (hov 1) (o [ fmt Format.[ !!(list ~sep:dot word) ] ])) lst

  let ipaddr_v4 = Format.using Ipaddr.V4.to_string Format.string
  let ipaddr_v6 = Format.using Ipaddr.V6.to_string Format.string

  let domain ppf = function
    | `Domain domain ->
      let x ppf x = keval ppf id (node (hov 0) (o [ fmt Format.[ !!string ] ])) x in
      keval ppf id (node (hov 1) (o [ fmt Format.[ !!(list ~sep:dot x) ] ])) domain
    | `Literal literal ->
      keval ppf id (node (hov 1) (o [ fmt Format.[ char $ '['; !!string; char $ ']' ] ])) literal
    | `Addr (IPv4 ip) ->
      keval ppf id (node (hov 1) (o [ fmt Format.[ char $ '['; !!ipaddr_v4; char $ ']' ] ])) ip
    | `Addr (IPv6 ip) ->
      keval ppf id (node (hov 1) (o [ fmt Format.[ char $ '['; string $ "IPv6:"; !!ipaddr_v6; char $ ']' ] ])) ip
    | `Addr (Ext (ldh, v)) ->
      keval ppf id (node (hov 1) (o [ fmt Format.[ char $ '['; !!string; char $ ':'; !!string; char $ ']' ] ])) ldh v

  let phrase ppf lst =
    let elt ppf = function
      | `Dot -> Format.char ppf '.'
      | `Word w -> word ppf w
      | `Encoded e -> Encoded_word.Encoder.encoded_word ppf e in
    let space ppf () = keval ppf id (o [ space ]) in
    keval ppf id (node (hov 1) (o [ fmt Format.[ !!(list ~sep:(space, ()) elt) ] ])) lst

  let mailbox ppf (t:Rfc5322.mailbox) =
    match t.Rfc5322.name, t.Rfc5322.domain with
    | Some name, (x, []) ->
      keval ppf id
        (node (hov 1) (o [ fmt Format.[ !!phrase ]; space; fmt Format.[ char $ '<'; !!local; char $ '@'; !!domain; char $ '>' ] ]))
        name t.Rfc5322.local x
    | None, (x, []) ->
      keval ppf id
        (node (hov 1) (o [ fmt Format.[ !!local ]; cut; fmt Format.[ char $ '@' ]; cut; fmt Format.[ !!domain ]; ]))
        t.Rfc5322.local x
    | name, (x, r) ->
      let domains ppf lst =
        let domain ppf x = keval ppf id (node (hov 1) (o [ fmt Format.[ char $ '@'; !!domain ] ])) x in
        let comma = (fun ppf () -> keval ppf id (o [ fmt Format.[ char $ ',' ]; cut ])), () in
        keval ppf id (node (hov 1) (o [ fmt Format.[ !!(list ~sep:comma domain) ] ])) lst in
      let phrase ppf x = keval ppf id (node (hov 1) (o [ fmt Format.[ !!phrase ]; space ])) x in

      keval ppf id
        (node (hov 1) (o [ fmt Format.[ !!(option phrase) ]
                         ; cut
                         ; fmt Format.[ char $ '<' ]
                         ; cut
                         ; fmt Format.[ !!domains; char $ ':' ]
                         ; cut
                         ; fmt Format.[ !!local ]
                         ; cut
                         ; fmt Format.[ char $ '@' ]
                         ; cut
                         ; fmt Format.[ !!domain ]
                         ; fmt Format.[ char $ '>' ] ]))
        name r t.Rfc5322.local x
end

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
  | IPv4 v -> Ipaddr.V4.pp ppf v
  | IPv6 v -> Ipaddr.V6.pp ppf v
  | Ext (ldh, value) -> Fmt.pf ppf "%s:%s" ldh value

let pp_domain ppf = function
  | `Domain l -> Fmt.list ~sep:Fmt.(const string ".") Fmt.string ppf l
  | `Literal s -> Fmt.pf ppf "[%s]" s
  | `Addr x -> Fmt.pf ppf "[%a]" pp_literal_domain x

let pp_local = Fmt.list ~sep:Fmt.(const string ".") pp_word

let pp : t Fmt.t =
 fun ppf x ->
  Fmt.pf ppf "{ @[<hov>name = %a;@ local = %a;@ domain = %a@] }"
    Fmt.(hvbox (Dump.option pp_phrase))
    x.Rfc5322.name
    Fmt.(hvbox pp_local)
    x.Rfc5322.local
    Fmt.(hvbox (Dump.pair pp_domain (Dump.list pp_domain)))
    x.Rfc5322.domain
