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
  (* XXX(dinosaure): RFC 5321 (2.4) explains:
     The local-part of a mailbox MUST BE treated as case sensitive. *)
  try List.for_all2 (equal_word ~sensitive:true) a b
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
                 ;('\\',   '\\')
                 ;('\x07', 'a')
                 ;('\b',   'b')
                 ;('\t',   't')
                 ;('\n',   'n')
                 ;('\x0b', 'v')
                 ;('\x0c', 'f')
                 ;('\r',   'r')
                 ;('"',    '"')] in
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
  if is_atext_valid_string raw then Ok (`Atom raw)
  else if is_qtext_valid_string raw then Ok (`String raw)
  else if is_utf8_valid_string raw then Ok (`String (escape_string raw))
  else Rresult.R.error_msgf "word %S does not respect standards" raw

module Encoder = struct
  include Encoder

  let atom = [ box; !!string; close ]
  let str = [ box; char $ '"'; !!string; char $ '"'; close ]

  let word ppf = function
    | `Atom x -> eval ppf atom x
    | `String x -> eval ppf str (escape_string x)

  let dot =
    (fun ppf () -> eval ppf [ cut; char $ '.'; cut ]), ()
  let comma =
    (fun ppf () -> eval ppf [ cut; char $ ','; cut ]), ()

  let local ppf lst =
    eval ppf [ box; !!(list ~sep:dot word); close ] lst

  let ipaddr_v4 = using Ipaddr.V4.to_string string
  let ipaddr_v6 = using Ipaddr.V6.to_string string

  let domain ppf = function
    | `Domain domain ->
      let boxed_string ppf x = eval ppf [ box; !!string; close ] x in
      eval ppf [ box; !!(list ~sep:dot boxed_string); close ] domain
    | `Literal literal ->
      eval ppf [ box; char $ '['; cut; !!string; cut; char $ ']'; close ] literal
    | `Addr (IPv4 ip) ->
      eval ppf [ box; char $ '['; cut; !!ipaddr_v4; cut; char $ ']'; close ] ip
    | `Addr (IPv6 ip) ->
      eval ppf [ box; char $ '['; cut; string $ "IPv6:"; cut; !!ipaddr_v6; cut; char $ ']'; close ] ip
    | `Addr (Ext (ldh, v)) ->
      eval ppf [ box; char $ '['; cut; !!string; cut; char $ ':'; cut; !!string; cut; char $ ']'; close ] ldh v

  let phrase ppf lst =
    let elt ppf = function
      | `Dot -> char ppf '.'
      | `Word w -> word ppf w
      | `Encoded e -> Encoded_word.Encoder.encoded_word ppf e in
    let space ppf () = eval ppf [ fws ] in
    eval ppf [ box; !!(list ~sep:(space, ()) elt); close ] lst

  let mailbox ppf (t:Rfc5322.mailbox) =
    match t.Rfc5322.name, t.Rfc5322.domain with
    | Some name, (x, []) ->
      eval ppf [ box; !!phrase ; spaces 1; char $ '<'; cut; !!local; cut; char $ '@'; cut; !!domain; cut; char $ '>'; close ]
        name t.Rfc5322.local x
    | None, (x, []) ->
      eval ppf [ box; !!local ; cut; char $ '@'; cut; !!domain; close ]
        t.Rfc5322.local x
    | name, (x, r) ->
      let domains ppf lst =
        let domain ppf x = eval ppf [ box; char $ '@'; !!domain; close ] x in
        let comma = (fun ppf () -> eval ppf [ char $ ','; cut ]), () in
        eval ppf [ box; !!(list ~sep:comma domain); close ] lst in
      let phrase ppf x = eval ppf [ box; !!phrase; spaces 1; close ] x in

      eval ppf
        [ box; !!(option phrase); cut; char $ '<'; cut; !!domains; cut; char $ ':'; cut; !!local; cut; char $ '@'; cut; !!domain; cut; char $ '>'; close ]
        name r t.Rfc5322.local x

  let mailboxes = list ~sep:comma mailbox
end

module Phrase = struct
  type elt = [`Word of Rfc822.word | `Encoded of Encoded_word.t | `Dot]
  type 'a t = [] : Peano.z t | ( :: ) : elt * 'a t -> 'a Peano.s t

  let o : elt = `Dot
  let q = Encoded_word.q
  let b = Encoded_word.b
  let word x : (elt, [ `Msg of string]) result = Rresult.R.(make_word x >>| fun x -> `Word x)

  let word_exn x : elt =
    match word x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let w : string -> elt = word_exn
  let e ~encoding v : elt = `Encoded (Encoded_word.make_exn ~encoding v)

  let rec coerce : type a. a Peano.s t -> phrase = function
    | [x] -> [(x :> elt)]
    | x :: y :: r -> List.cons (x :> elt) (coerce (y :: r))

  let make : type a. a t -> (phrase, [ `Msg of string ]) result = function
    | [] -> Rresult.R.error_msgf "A phrase must contain at least one element"
    | x :: r -> Ok (coerce (x :: r))

  let v l =
    match make l with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let to_string x = Encoder.to_string Encoder.phrase x
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

  let make : type a. a t -> a -> (literal_domain, [ `Msg of string ]) result =
   fun witness v ->
    match witness with
    | IPv4 -> Ok (Rfc5321.IPv4 v)
    | IPv6 -> Ok (Rfc5321.IPv6 v)
    | Ext ->
        let ldh, value = v in
        if is_ldh_valid_string ldh && is_dcontent_valid_string value then
          Ok (Rfc5321.Ext (ldh, value))
        else Rresult.R.error_msgf "literal-domain %S-%S does not respect standards" ldh value

  let v : type a. a t -> a -> literal_domain =
   fun witness v ->
    match make witness v with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err
end

module Domain = struct
  let atom x = if is_atext_valid_string x then Ok (`Atom x) else Rresult.R.error_msgf "atom %S does not respect standards" x

  let atom_exn x =
    match atom x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

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
    if is_dtext_valid_string x then Ok (`Literal x)
    else if is_utf8_valid_string x then Ok (`Literal (escape_string x))
    else Rresult.R.error_msgf "literal domain %S does not respect standards" x

  let literal_exn x =
    match literal x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  type atom = [`Atom of string]
  type literal = [`Literal of string]

  type 'a domain =
    | ( :: ) : atom * 'a domain -> 'a Peano.s domain
    | [] : Peano.z domain

  let rec coerce : type a. a Peano.s domain -> string list = function
    | [`Atom x] -> [x]
    | `Atom x :: y :: r -> List.cons x (coerce (y :: r))

  let make_domain : type a. a domain -> (string list, [ `Msg of string ]) result = function
    | [] -> Rresult.R.error_msg "A domain must contain at least one element"
    | x :: r -> Ok (coerce (x :: r))

  type 'a t =
    | Domain : 'a domain t
    | Literal_domain : 'a Literal_domain.t -> 'a t
    | Literal : string t

  let domain = Domain
  let ipv4 = Literal_domain Literal_domain.IPv4
  let ipv6 = Literal_domain Literal_domain.IPv6
  let extension = Literal_domain Literal_domain.Ext
  let default = Literal

  let make : type a. a t -> a -> (Rfc5322.domain, [ `Msg of string ]) result =
   fun witness v ->
    match witness with
    | Domain -> Rresult.R.(make_domain v >>| fun v -> `Domain v)
    | Literal_domain witness ->
        Rresult.R.(Literal_domain.make witness v >>| fun v -> `Addr v)
    | Literal -> literal v

  let v : type a. a t -> a -> Rfc5322.domain =
   fun witness v ->
    match make witness v with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let to_string x = Encoder.to_string Encoder.domain x
end

module Local = struct
  type 'a local =
    | [] : Peano.z local
    | ( :: ) : word * 'a local -> 'a Peano.s local

  let word x = if String.length x > 0 then make_word x else Rresult.R.error_msgf "A word can not be empty"

  let word_exn x =
    match word x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let w = word_exn

  let rec coerce : type a. a Peano.s local -> Rfc822.local = function
    | [x] -> List.cons x []
    | x :: y :: r -> List.cons x (coerce (y :: r))

  let make : type a. a local -> (Rfc822.local, [ `Msg of string ]) result = function
    | [] -> Rresult.R.error_msg "A local-part must contain at least one element"
    | x :: r -> Ok (coerce (x :: r))

  let v : type a. a local -> Rfc822.local =
   fun l ->
    match make l with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let to_string x = Encoder.to_string Encoder.local x
end

let make ?name local ?(domains= []) domain =
  { name; local; domain= (domain, domains); }

let ( @ ) : 'a Local.local -> 'b Domain.t * 'b -> Rfc5322.mailbox =
 fun local (witness, domain) ->
  match (Local.make local, Domain.make witness domain) with
  | Ok local, Ok domain ->
      {Rfc5322.name= None; local; domain= (domain, [])}
  | Error (`Msg err), Ok _ -> invalid_arg err
  | Ok _, Error (`Msg err) -> invalid_arg err
  | Error _, Error _ -> Fmt.invalid_arg "Invalid local-part and domain"

let with_name : Rfc5322.phrase -> Rfc5322.mailbox -> Rfc5322.mailbox =
 fun name mailbox -> {mailbox with Rfc5322.name= Some name}

let to_string x = Encoder.to_string Encoder.mailbox x

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

let mailboxes_to_unstructured ~field_name x =
  Unstructured.to_unstructured ~field_name Encoder.mailboxes x

let to_unstructured ~field_name x =
  Unstructured.to_unstructured ~field_name Encoder.mailbox x
