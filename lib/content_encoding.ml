type t = Rfc2045.mechanism

let pp ppf = function
  | `Bit7 -> Fmt.string ppf "7bit"
  | `Bit8 -> Fmt.string ppf "8bit"
  | `Binary -> Fmt.string ppf "binary"
  | `Quoted_printable -> Fmt.string ppf "quoted-printable"
  | `Base64 -> Fmt.string ppf "base64"
  | `Ietf_token token -> Fmt.pf ppf "ietf:%s" token
  | `X_token token -> Fmt.pf ppf "x:%s" token

let default = `Bit7

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let mechanism ppf = function
    | `Bit7 -> string ppf "7bit"
    | `Bit8 -> string ppf "8bit"
    | `Binary -> string ppf "binary"
    | `Quoted_printable -> string ppf "quoted-printable"
    | `Base64 -> string ppf "base64"
    | `Ietf_token x -> string ppf x
    | `X_token x -> keval ppf id [ string $ "X-"; !!string ] x
end
