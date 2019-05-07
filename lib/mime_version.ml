type t = Rfc2045.version

let pp = Fmt.Dump.pair Fmt.int Fmt.int

let default = (1, 0)

module Encoder = struct
  open Encoder

  external id : 'a -> 'a = "%identity"

  let int = using string_of_int string

  let mime_version ppf (a, b) =
    eval ppf [ !!int; char $ '.'; !!int ] a b
end
