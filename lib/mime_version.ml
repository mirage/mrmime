type t = Rfc2045.version

let pp = Fmt.Dump.pair Fmt.int Fmt.int

let default = (1, 0)
