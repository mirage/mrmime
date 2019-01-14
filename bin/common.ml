open Cmdliner

type newline =
  | CRLF | LF

let newline =
  let parser str = match String.lowercase_ascii str with
    | "crlf" -> Ok CRLF
    | "lf" -> Ok LF
    | str -> Rresult.R.error_msgf "Invalid newline: %s" str in
  let pp ppf = function
    | CRLF -> Fmt.string ppf "<CRLF>"
    | LF -> Fmt.string ppf "<LF>" in
  Arg.conv ~docv:"<newline>" (parser, pp)

let newline =
  let doc = "Newline." in
  Arg.(value & opt newline LF & info [ "n"; "newline" ] ~docv:"<newline>" ~doc)

let sub_string_and_replace_newline chunk len =
  let count = ref 0 in
  String.iter (function '\n' -> incr count | _ -> ()) (Bytes.sub_string chunk 0 len) ;
  let plus = !count in
  let pos = ref 0 in
  let res = Bytes.create (len + plus) in
  for i = 0 to len - 1
  do match Bytes.unsafe_get chunk i with
    | '\n' ->
      Bytes.unsafe_set res !pos '\r' ;
      Bytes.unsafe_set res (!pos + 1) '\n' ;
      pos := !pos + 2
    | chr ->
      Bytes.unsafe_set res !pos chr ;
      incr pos
  done ; Bytes.unsafe_to_string res

let sanitize_input newline chunk len = match newline with
  | CRLF -> Bytes.sub_string chunk 0 len
  | LF -> sub_string_and_replace_newline chunk len
