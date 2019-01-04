type t = string

let pp_char =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp : t Fmt.t = fun ppf x ->
  let l = String.length x in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ String.get x ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_char (String.get x ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

