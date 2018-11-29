open Angstrom

let is_ascii = function '\000' .. '\127' -> true | _ -> false

exception Satisfy

type 'a s = string

let with_uutf is : [`UTF_8] s t =
  let res = Buffer.create 16 in
  let tmp = Bytes.create 1 in
  let dec = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let cut = ref false in
  scan (Uutf.decode dec) (fun state chr ->
      try
        let () =
          match state with
          | `Await | `End -> ()
          | `Malformed _ -> Uutf.Buffer.add_utf_8 res Uutf.u_rep
          | `Uchar uchar when Uchar.is_char uchar ->
              if is (Uchar.to_char uchar) then
                Buffer.add_char res (Uchar.to_char uchar)
              else raise Satisfy
          | `Uchar uchar -> Uutf.Buffer.add_utf_8 res uchar
        in
        Bytes.set tmp 0 chr ;
        Uutf.Manual.src dec tmp 0 1 ;
        if is_ascii chr && not (is chr) then (
          cut := true ;
          raise Satisfy ) ;
        Some (Uutf.decode dec)
      with Satisfy -> None )
  >>= fun (_, state) ->
  ( match state with
  | `Await ->
      Uutf.Manual.src dec tmp 0 1 ;
      let () =
        match Uutf.decode dec with
        | `Await | `Malformed _ -> Uutf.Buffer.add_utf_8 res Uutf.u_rep
        | `Uchar uchar when Uchar.is_char uchar ->
            if is (Uchar.to_char uchar) then
              Buffer.add_char res (Uchar.to_char uchar)
        | `Uchar uchar -> Uutf.Buffer.add_utf_8 res uchar
        | `End -> ()
      in
      return (Buffer.contents res)
  | `Malformed _ ->
      Uutf.Buffer.add_utf_8 res Uutf.u_rep ;
      return (Buffer.contents res)
  | `Uchar uchar when Uchar.is_char uchar ->
      if (not !cut) && is (Uchar.to_char uchar) then
        Buffer.add_char res (Uchar.to_char uchar) ;
      return (Buffer.contents res)
  | `Uchar uchar ->
      Uutf.Buffer.add_utf_8 res uchar ;
      return (Buffer.contents res)
  | `End -> return (Buffer.contents res) )
  >>= fun r -> Buffer.clear res ; return r

let with_uutf1 is : [`UTF_8] s t =
  with_uutf is
  (* TODO: should use something else than [String.length]. *)
  >>= fun r -> if String.length r > 0 then return r else fail "with_uutf1"
