type t = Emile.mailbox

let pp = Emile.pp_mailbox
let equal = Emile.equal_mailbox
let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

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

let is_atext_valid_string = is_utf8_valid_string_with Emile.Parser.is_atext
let is_dtext_valid_string = is_utf8_valid_string_with Emile.Parser.is_dtext
let is_qtext_valid_string = is_utf8_valid_string_with Emile.Parser.is_qtext

let need_to_escape, escape_char =
  (* See [of_escaped_character] but totally arbitrary. *)
  let bindings =
    [ ('\000', '\000');
      ('\\', '\\');
      ('\x07', 'a');
      ('\b', 'b');
      ('\t', 't');
      ('\n', 'n');
      ('\x0b', 'v');
      ('\x0c', 'f');
      ('\r', 'r');
      ('"', '"')
    ]
  in
  ((fun chr -> List.mem_assoc chr bindings), fun chr -> List.assoc chr bindings)

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

let make_word raw =
  if is_atext_valid_string raw then Ok (`Atom raw)
  else if is_qtext_valid_string raw then Ok (`String raw)
  else if is_utf8_valid_string raw then Ok (`String (escape_string raw))
  else error_msgf "word %S does not respect standards" raw

module Decoder = struct
  let mailbox = Emile.Parser.mailbox
  let mailbox_list = Emile.Parser.mailbox_list
end

module Encoder = struct
  open Prettym

  let atom = [ !!string ]
  let str = [ box; char $ '"'; !!string; char $ '"'; close ]

  let word ppf = function
    | `Atom x -> eval ppf atom x
    | `String x -> eval ppf str (escape_string x)

  let cut : type a. unit -> (a, a) order = fun () -> break ~indent:1 ~len:0
  (* XXX(dinosaure): we must ensure that a break insert (if we really apply
   * it) a space at the beginning of the new line! [Prettym.cut] does not do
   * that, it only gives the opportunity to break a new line without indentation.
   *
   * It safe to use [Prettym.cut] only inside a [tbox] in this context - to not
   * really break the mailbox. Note that such point is due to the release of
   * [prettym.0.0.1] which slightly change the semantic of [fws] and [cut]. *)

  let dot = ((fun ppf () -> eval ppf [ cut (); char $ '.'; cut () ]), ())
  let comma = ((fun ppf () -> eval ppf [ cut (); char $ ','; cut () ]), ())
  let local ppf lst = eval ppf [ box; !!(list ~sep:dot word); close ] lst
  let ipaddr_v4 = using Ipaddr.V4.to_string string
  let ipaddr_v6 = using Ipaddr.V6.to_string string

  let domain ppf = function
    | `Domain domain ->
        let boxed_string ppf x = eval ppf [ box; !!string; close ] x in
        eval ppf [ box; !!(list ~sep:dot boxed_string); close ] domain
    | `Literal literal ->
        eval ppf
          [ box; char $ '['; cut (); !!string; cut (); char $ ']'; close ]
          literal
    | `Addr (Emile.IPv4 ip) ->
        eval ppf
          [ box; char $ '['; cut (); !!ipaddr_v4; cut (); char $ ']'; close ]
          ip
    | `Addr (Emile.IPv6 ip) ->
        eval ppf
          [ box;
            char $ '[';
            cut ();
            string $ "IPv6:";
            cut ();
            !!ipaddr_v6;
            cut ();
            char $ ']';
            close
          ]
          ip
    | `Addr (Emile.Ext (ldh, v)) ->
        eval ppf
          [ box;
            char $ '[';
            cut ();
            !!string;
            cut ();
            char $ ':';
            cut ();
            !!string;
            cut ();
            char $ ']';
            close
          ]
          ldh v

  let phrase ppf lst =
    let elt ppf = function
      | `Dot -> char ppf '.'
      | `Word w -> word ppf w
      | `Encoded (charset, Emile.Quoted_printable data) ->
          Encoded_word.Encoder.encoded_word ppf
            { Encoded_word.charset = `Charset charset;
              encoding = Encoded_word.Quoted_printable;
              data
            }
      | `Encoded (charset, Emile.Base64 data) ->
          Encoded_word.Encoder.encoded_word ppf
            { Encoded_word.charset = `Charset charset;
              encoding = Encoded_word.Base64;
              data
            }
    in
    let space ppf () = eval ppf [ fws ] in
    eval ppf [ box; !!(list ~sep:(space, ()) elt); close ] lst

  let mailbox ppf (t : Emile.mailbox) =
    match (t.Emile.name, t.Emile.domain) with
    | Some name, (x, []) ->
        eval ppf
          [ box;
            !!phrase;
            spaces 1;
            char $ '<';
            cut ();
            !!local;
            cut ();
            char $ '@';
            cut ();
            !!domain;
            cut ();
            char $ '>';
            close
          ]
          name t.Emile.local x
    | None, (x, []) ->
        eval ppf
          [ !!local; cut (); char $ '@'; cut (); !!domain ]
          t.Emile.local x
    | name, (x, r) ->
        let domains ppf lst =
          let domain ppf x = eval ppf [ box; char $ '@'; !!domain; close ] x in
          (* XXX(dinosaure): according RFC, comma is surrounded by CFWS. *)
          let comma =
            ((fun ppf () -> eval ppf [ cut (); char $ ','; cut () ]), ())
          in
          eval ppf [ box; !!(list ~sep:comma domain); close ] lst
        in
        let phrase ppf x = eval ppf [ box; !!phrase; spaces 1; close ] x in

        eval ppf
          [ box;
            !!(option phrase);
            cut ();
            char $ '<';
            cut ();
            !!domains;
            cut ();
            char $ ':';
            cut ();
            !!local;
            cut ();
            char $ '@';
            cut ();
            !!domain;
            cut ();
            char $ '>';
            close
          ]
          name r t.Emile.local x

  let mailboxes = list ~sep:comma mailbox
end

module Phrase = struct
  type elt = [ `Word of Emile.word | `Encoded of string * Emile.raw | `Dot ]
  type 'a t = [] : Peano.z t | ( :: ) : elt * 'a t -> 'a Peano.s t

  let o : elt = `Dot
  let q = Encoded_word.q
  let b = Encoded_word.b

  let word x : (elt, [> `Msg of string ]) result =
    Result.map (fun x -> `Word x) (make_word x)

  let word_exn x : elt =
    match word x with Ok v -> v | Error (`Msg err) -> invalid_arg err

  let w : string -> elt = word_exn

  let e ~encoding v : elt =
    let x = Encoded_word.make_exn ~encoding v in
    let charset = Encoded_word.charset_to_string x.Encoded_word.charset in
    match x.Encoded_word.encoding with
    | Base64 -> `Encoded (charset, Emile.Base64 x.Encoded_word.data)
    | Quoted_printable ->
        `Encoded (charset, Emile.Quoted_printable x.Encoded_word.data)

  let rec coerce : type a. a Peano.s t -> Emile.phrase = function
    | [ x ] -> [ (x :> elt) ]
    | x :: y :: r -> List.cons (x :> elt) (coerce (y :: r))

  let make : type a. a t -> (Emile.phrase, [> `Msg of string ]) result =
    function
    | [] -> error_msgf "A phrase must contain at least one element"
    | x :: r -> Ok (coerce (x :: r))

  let v l = match make l with Ok v -> v | Error (`Msg err) -> invalid_arg err
  let to_string x = Prettym.to_string Encoder.phrase x
end

module Literal_domain = struct
  type 'a t =
    | IPv4 : Ipaddr.V4.t t
    | IPv6 : Ipaddr.V6.t t
    | Ext : (string * string) t

  let is_ldh_valid_string x =
    let exception Invalid_char in
    try
      let len = String.length x in
      String.iteri
        (fun pos -> function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> ()
          | '-' -> if pos = len - 1 then raise Invalid_char (* else () *)
          | _ -> raise Invalid_char)
        x;
      true
    with Invalid_char -> false

  let is_dcontent = function
    | '\033' .. '\090' | '\094' .. '\126' -> true
    | _ -> false

  let is_dcontent_valid_string x =
    let exception Invalid_char in
    try
      String.iter
        (fun chr -> if not (is_dcontent chr) then raise Invalid_char)
        x;
      true
    with Invalid_char -> false

  let ipv4 = IPv4
  let ipv6 = IPv6
  let extension = Ext

  let make : type a. a t -> a -> (Emile.addr, [> `Msg of string ]) result =
   fun witness v ->
    match witness with
    | IPv4 -> Ok (Emile.IPv4 v)
    | IPv6 -> Ok (Emile.IPv6 v)
    | Ext ->
        let ldh, value = v in
        if is_ldh_valid_string ldh && is_dcontent_valid_string value then
          Ok (Emile.Ext (ldh, value))
        else
          error_msgf "literal-domain %S-%S does not respect standards" ldh value

  let v : type a. a t -> a -> Emile.addr =
   fun witness v ->
    match make witness v with Ok v -> v | Error (`Msg err) -> invalid_arg err
end

module Domain = struct
  let atom x =
    if is_atext_valid_string x then Ok (`Atom x)
    else error_msgf "atom %S does not respect standards" x

  let atom_exn x =
    match atom x with Ok v -> v | Error (`Msg err) -> invalid_arg err

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
    if is_dtext_valid_string x then Ok (`Literal x)
    else if is_utf8_valid_string x then Ok (`Literal (escape_string x))
    else error_msgf "literal domain %S does not respect standards" x

  let literal_exn x =
    match literal x with Ok v -> v | Error (`Msg err) -> invalid_arg err

  type atom = [ `Atom of string ]
  type literal = [ `Literal of string ]

  let of_list l =
    let l = List.map atom l in
    let l =
      List.fold_left
        (fun a x ->
          match (a, x) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok a, Ok (`Atom x) -> Ok (x :: a))
        (Ok []) l
    in
    let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
    l >>| List.rev >>= function
    | [] -> error_msgf "A domain must contain at least one element"
    | v -> Ok (`Domain v)

  type 'a domain =
    | ( :: ) : atom * 'a domain -> 'a Peano.s domain
    | [] : Peano.z domain

  let rec coerce : type a. a Peano.s domain -> string list = function
    | [ `Atom x ] -> [ x ]
    | `Atom x :: y :: r -> List.cons x (coerce (y :: r))

  let make_domain :
      type a. a domain -> (string list, [> `Msg of string ]) result = function
    | [] -> error_msgf "A domain must contain at least one element"
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

  let make : type a. a t -> a -> (Emile.domain, [> `Msg of string ]) result =
   fun witness v ->
    match witness with
    | Domain -> Result.map (fun v -> `Domain v) (make_domain v)
    | Literal_domain witness ->
        Result.map (fun v -> `Addr v) (Literal_domain.make witness v)
    | Literal -> literal v

  let v : type a. a t -> a -> Emile.domain =
   fun witness v ->
    match make witness v with Ok v -> v | Error (`Msg err) -> invalid_arg err

  let to_string x = Prettym.to_string Encoder.domain x
end

module Local = struct
  type 'a local =
    | [] : Peano.z local
    | ( :: ) : Emile.word * 'a local -> 'a Peano.s local

  let word x =
    if String.length x > 0 then make_word x
    else error_msgf "A word can not be empty"

  let word_exn x =
    match word x with Ok v -> v | Error (`Msg err) -> invalid_arg err

  let w = word_exn

  let rec coerce : type a. a Peano.s local -> Emile.local = function
    | [ x ] -> List.cons x []
    | x :: y :: r -> List.cons x (coerce (y :: r))

  let make : type a. a local -> (Emile.local, [> `Msg of string ]) result =
    function
    | [] -> error_msgf "A local-part must contain at least one element"
    | x :: r -> Ok (coerce (x :: r))

  let of_list l =
    let l = List.map word l in
    let l =
      List.fold_left
        (fun a x ->
          match (a, x) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok a, Ok x -> Ok (List.cons x a))
        (Ok []) l
    in
    let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
    l >>| List.rev >>= function
    | [] -> error_msgf "A local-part must contain at least one element"
    | v -> Ok v

  let v : type a. a local -> Emile.local =
   fun l -> match make l with Ok v -> v | Error (`Msg err) -> invalid_arg err

  let to_string x = Prettym.to_string Encoder.local x
end

let make ?name local ?(domains = []) domain =
  { Emile.name; local; domain = (domain, domains) }

let ( @ ) : 'a Local.local -> 'b Domain.t * 'b -> Emile.mailbox =
 fun local (witness, domain) ->
  match (Local.make local, Domain.make witness domain) with
  | Ok local, Ok domain -> { Emile.name = None; local; domain = (domain, []) }
  | Error (`Msg err), Ok _ -> invalid_arg err
  | Ok _, Error (`Msg err) -> invalid_arg err
  | Error _, Error _ -> invalid_arg "Invalid local-part and domain"

let with_name name mailbox = { mailbox with Emile.name = Some name }

let of_string x =
  match Emile.of_string x with
  | Ok v -> Ok v
  | Error (`Invalid _) -> error_msgf "Invalid email address: %S" x

let to_string = Prettym.to_string Encoder.mailbox
