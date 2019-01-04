type t = Rfc5322.address

let pp : t Fmt.t = fun ppf t -> match t with
  | `Group t -> Fmt.pf ppf "@[<hov>(Group@ %a)@]" Group.pp t
  | `Mailbox t -> Fmt.pf ppf "@[<hov>(Mailbox@ %a)@]" Mailbox.pp t
