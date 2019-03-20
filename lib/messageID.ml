type word = [`Atom of string | `String of string]
type local = word list
type domain = [`Domain of string list | `Literal of string]
type t = Rfc822.nonsense Rfc822.msg_id (* = local * domain *)

let pp_word ppf = function
  | `Atom x -> Fmt.string ppf x
  | `String x -> Fmt.pf ppf "%S" x

let pp_domain ppf = function
  | `Domain l -> Fmt.list ~sep:Fmt.(const string ".") Fmt.string ppf l
  | `Literal x -> Fmt.pf ppf "[%s]" x
  | `Addr _ -> assert false

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
  open Encoder

  external id : 'a -> 'a = "%identity"

  let dot = using (fun () -> '.') char, ()

  let domain ppf = function
    | `Domain domain ->
      let x ppf x = keval ppf id [ hov 0; !!string; close ] x in
      keval ppf id [ hov 1; !!(list ~sep:dot x); close ] domain
    | `Literal literal ->
      keval ppf id [ hov 1; char $ '['; !!string; char $ ']'; close ] literal
    | `Addr Rfc822.Nonsense.Nonsense -> assert false

  let message_id ppf (t:Rfc822.nonsense Rfc822.msg_id) =
    match t with
    | (local_part, domain_part) ->
      keval ppf id [ hov 1; char $ '<'; !!Mailbox.Encoder.local; char $ '@'; !!domain; char $ '>'; close ]
        local_part domain_part
end
