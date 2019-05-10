let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp_string = pp_scalar ~get:String.get ~length:String.length
let pp_bytes = pp_scalar ~get:Bytes.get ~length:Bytes.length
let pp_bigstring = pp_scalar ~get:Bigstringaf.get ~length:Bigstringaf.length

let show ~on : unit Angstrom.t =
  let open Angstrom in
  available >>= fun len -> Unsafe.peek len
  @@ fun ba ~off ~len ->
  Fmt.epr "@[<hov>%a:@ %a@\n@].\n%!" on () (Fmt.hvbox pp_bigstring) (Bigstringaf.sub ba ~off ~len)

let is_utf8_valid_string_with is x =
  let exception Invalid_utf8 in
  let exception Invalid_char in
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function `Malformed _ -> raise Invalid_utf8
        | `Uchar uchar ->
            if Uchar.is_char uchar && not (is (Uchar.to_char uchar)) then
              raise Invalid_char )
      () x ;
    true
  with
  | Invalid_utf8 -> false
  | Invalid_char -> false

let is_utf8_valid_string x =
  let exception Invalid_utf8 in
  try
    Uutf.String.fold_utf_8
      (fun () _pos -> function `Malformed _ -> raise Invalid_utf8 | _ -> ())
      () x ;
    true
  with Invalid_utf8 -> false


