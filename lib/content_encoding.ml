type t = Rfc2045.mechanism
type field = [ `ContentEncoding of t ]

let pp ppf = function
  | `Bit7 -> Fmt.string ppf "7bit"
  | `Bit8 -> Fmt.string ppf "8bit"
  | `Binary -> Fmt.string ppf "binary"
  | `Quoted_printable -> Fmt.string ppf "quoted-printable"
  | `Base64 -> Fmt.string ppf "base64"
  | `Ietf_token token -> Fmt.pf ppf "ietf:%s" token
  | `X_token token -> Fmt.pf ppf "x:%s" token

let default = `Bit7
