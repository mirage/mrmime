type t = Rfc5322.unstructured

let pp_atom ppf = function
  | `Text x -> Fmt.quote Fmt.string ppf x
  | `WSP -> Fmt.pf ppf "@ "
  | `CR n -> Fmt.pf ppf "<CR:%d>" n
  | `LF n -> Fmt.pf ppf "<LF:%d>" n
  | `CRLF -> Fmt.pf ppf "<CRLF>@\n"
  | `Encoded t -> Encoded_word.pp ppf t

let pp : t Fmt.t = Fmt.list ~sep:(Fmt.always "@,") pp_atom
