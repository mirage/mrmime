type t = Rfc5322.unstructured

let pp_atom ppf = function
  | `Text x -> Fmt.quote Fmt.string ppf x
  | `WSP wsp -> Fmt.pf ppf "<WSP:%s>"wsp
  | `CR n -> Fmt.pf ppf "<CR:%d>" n
  | `LF n -> Fmt.pf ppf "<LF:%d>" n
  | `CRLF -> Fmt.pf ppf "<CRLF>@\n"
  | `Encoded t -> Encoded_word.pp ppf t

let pp : t Fmt.t = Fmt.list ~sep:(Fmt.always "@,") pp_atom

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let element ppf = function
    | `Text x -> string ppf x
    | `WSP x -> string ppf x
    | `CR n -> string ppf (String.make n '\r')
    | `LF n -> string ppf (String.make n '\n')
    | `CRLF -> string ppf "\r\n"
    | `Encoded x -> Encoded_word.Encoder.encoded_word ppf x

  let cut = (fun ppf () -> keval ppf id [ cut ]), ()
  let unstructured : Rfc5322.unstructured encoding = list ~sep:cut element
end
