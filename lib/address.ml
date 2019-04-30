type t = Rfc5322.address

let group group = `Group group
let mailbox mailbox = `Mailbox mailbox

let equal a b = match a, b with
  | `Group a, `Group b -> Group.equal a b
  | `Mailbox a, `Mailbox b -> Mailbox.equal a b
  | _, _ -> false

let pp : t Fmt.t = fun ppf t -> match t with
  | `Group t -> Fmt.pf ppf "@[<hov>(Group@ %a)@]" Group.pp t
  | `Mailbox t -> Fmt.pf ppf "@[<hov>(Mailbox@ %a)@]" Mailbox.pp t

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let mailbox = Mailbox.Encoder.mailbox
  let group = Group.Encoder.group
  let comma = (fun ppf () -> keval ppf id [ char $ ','; space ]), ()

  let address ppf = function
    | `Mailbox m -> mailbox ppf m
    | `Group g -> group ppf g

  let addresses ppf l =
    keval ppf id [ hov 1; !!(list ~sep:comma address); close ] l
end

let addresses_to_unstructured ~field_name x =
  Unstructured.to_unstructured ~field_name Encoder.addresses x

let to_unstructured ~field_name x =
  Unstructured.to_unstructured ~field_name Encoder.address x
