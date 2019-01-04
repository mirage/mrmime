type word = [`Atom of string | `String of string]
type phrase = [`Dot | `Word of word | `Encoded of Encoded_word.t] list

type t = Rfc5322.group =
  { name : phrase
  ; mailboxes : Mailbox.t list }

let pp : t Fmt.t = fun ppf t ->
  Fmt.pf ppf "{ @[<hov>name = %a;@ \
                       mailboxes = %a;@] }"
    Mailbox.pp_phrase t.name
    (Fmt.Dump.list Mailbox.pp) t.mailboxes
