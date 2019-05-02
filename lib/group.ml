type word = [`Atom of string | `String of string]
type phrase = [`Dot | `Word of word | `Encoded of Encoded_word.t] list

type t = Rfc5322.group =
  { name : phrase
  ; mailboxes : Mailbox.t list }

let equal a b =
  let mailboxes =
    try List.for_all2 Mailbox.equal a.mailboxes b.mailboxes
    with _ -> false in
  Mailbox.equal_phrase a.name b.name
  && mailboxes

let make ~name mailboxes =
  if List.length mailboxes = 0 then None
  else Some { name; mailboxes; }

let v ~name mailboxes = match make ~name mailboxes with
  | None -> Fmt.invalid_arg "A group contains at least one mailbox"
  | Some t -> t

let pp : t Fmt.t = fun ppf t ->
  Fmt.pf ppf "{ @[<hov>name = %a;@ \
                       mailboxes = %a;@] }"
    Mailbox.pp_phrase t.name
    (Fmt.Dump.list Mailbox.pp) t.mailboxes

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let comma = (fun ppf () -> keval ppf id [ char $ ','; space ]), ()
  let phrase = Mailbox.Encoder.phrase
  let mailbox = Mailbox.Encoder.mailbox

  let group ppf t =
    keval ppf id [ hov 1; !!phrase; char $ ':'; space; hov 1; !!(list ~sep:comma mailbox); close; char $ ';'; close ]
      t.name t.mailboxes
end
