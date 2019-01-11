open Angstrom

let is_ascii = function '\000' .. '\127' -> true | _ -> false

exception Not_satisfy

type t =
  { normalized : string
  ; raw : string }

let failf = Fmt.kstrf fail

let with_uutf is =
  let res = Buffer.create 16 in
  let raw = Buffer.create 16 in
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
              else raise Not_satisfy
          | `Uchar uchar -> Uutf.Buffer.add_utf_8 res uchar
        in
        Bytes.set tmp 0 chr ;
        Uutf.Manual.src dec tmp 0 1 ;
        if is_ascii chr && not (is chr) then (
          cut := true ;
          raise Not_satisfy ) ;
        Buffer.add_char raw chr ; (* valid [char]. *)
        Some (Uutf.decode dec)
      with Not_satisfy -> None )
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
      return { normalized= Buffer.contents res
             ; raw= Buffer.contents raw }
  | `Malformed _ ->
      Uutf.Buffer.add_utf_8 res Uutf.u_rep ;
      return { normalized= Buffer.contents res
             ; raw= Buffer.contents raw }
  | `Uchar uchar when Uchar.is_char uchar ->
      if (not !cut) && is (Uchar.to_char uchar) then
        Buffer.add_char res (Uchar.to_char uchar) ;
      return { normalized= Buffer.contents res
             ; raw= Buffer.contents raw }
  | `Uchar uchar ->
      Uutf.Buffer.add_utf_8 res uchar ;
      return { normalized= Buffer.contents res
             ; raw= Buffer.contents raw }
  | `End -> return { normalized= Buffer.contents res
                   ; raw= Buffer.contents raw } )
  >>= fun r -> Buffer.clear res ; Buffer.clear raw ; return r

let with_uutf_without_raw is =
  with_uutf is >>| fun { normalized; _ } -> normalized

let with_uutf1 is =
  with_uutf is
  >>= fun r ->
  if String.length r.raw > 0
  then return r
  else failf "with_uutf1: string is empty"

let with_uutf1_without_raw is =
  with_uutf1 is >>| fun { normalized; _ } -> normalized
