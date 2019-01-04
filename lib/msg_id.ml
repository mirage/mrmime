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
