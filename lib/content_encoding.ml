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

let of_string = function
  | "7bit" -> Ok `Bit7
  | "8bit" -> Ok `Bit8
  | "binary" -> Ok `Binary
  | "quoted-printable" -> Ok `Quoted_printable
  | "base64" -> Ok `Base64
  | x -> Rresult.R.error_msgf "Invalid MIME encoding: %s" x
(* TODO:
   - let the user to craft an extension token.
   - check IETF database *)

module Encoder = struct
  open Encoder

  let mechanism ppf = function
    | `Bit7 -> string ppf "7bit"
    | `Bit8 -> string ppf "8bit"
    | `Binary -> string ppf "binary"
    | `Quoted_printable -> string ppf "quoted-printable"
    | `Base64 -> string ppf "base64"
    | `Ietf_token x -> string ppf x
    | `X_token x -> eval ppf [ string $ "X-"; !!string ] x
end
