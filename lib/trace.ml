type field  = Rfc5322.trace

type word = Rfc822.word
type local = Rfc822.local
type phrase = Rfc5322.phrase

type literal_domain = Rfc5321.literal_domain =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Ext of string * string

type domain = Rfc5322.domain

type mailbox = Rfc5322.mailbox =
  { name : phrase option
  ; local : local
  ; domain : domain * domain list }

type t =
  { trace : mailbox option
  ; received : (received list * Date.t option) list }
and received =
  [ `Addr of mailbox
  | `Domain of domain
  | `Word of word ]

let pp_trace ppf (local, (x, r)) = match r with
  | [] ->
    Fmt.pf ppf "{ @[<hov>local = %a;@ domain = %a@] }"
      Mailbox.pp_local local Mailbox.pp_domain x
  | domains ->
    Fmt.pf ppf "{ @[<hov>local = %a;@ domains = %a@] }"
      Mailbox.pp_local local
      Fmt.(hvbox (Dump.list Mailbox.pp_domain)) (x :: domains)

let pp_trace = Fmt.using (fun { local; domain; _} -> local, domain) pp_trace

let pp_received ppf = function
  | `Addr v -> Fmt.pf ppf "(`Addr %a)" (Fmt.hvbox pp_trace) v
  | `Domain v -> Fmt.pf ppf "(`Domain %a)" (Fmt.hvbox Mailbox.pp_domain) v
  | `Word v -> Fmt.pf ppf "(`Word %a)" (Fmt.hvbox Mailbox.pp_word) v

let pp_received ppf = function
  | received, Some date ->
    Fmt.pf ppf "{ @[<hov>received = %a;@ date = %a;@] }"
      Fmt.(Dump.list pp_received) received
      Date.pp date
  | received, None ->
    Fmt.pf ppf "{ @[<hov>received = %a;@] }"
      Fmt.(Dump.list pp_received) received

let pp ppf = function
  | { trace= Some trace; received; } ->
    Fmt.pf ppf "{ @[<hov>trace = %a;@ received = %a;@] }"
      pp_trace trace
      Fmt.(vbox (list ~sep:(always "@\n&@ ") pp_received)) received
  | { received; _ } ->
    Fmt.pf ppf "{ @[<hov>received = %a;@] }"
      Fmt.(vbox (list ~sep:(always "@\n&@ ") pp_received)) received

let fold : ([> field ] as 'a) list -> t list -> (t list * 'a list) = fun fields t ->
  List.fold_left
    (fun (t, rest) -> function
       | `Trace (trace, received) ->
         { trace; received; } :: t, rest
       | field -> t, field :: rest)
    (t, []) fields
  |> fun (t, fields) -> t, List.rev fields
