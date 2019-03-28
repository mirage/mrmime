open Angstrom

let is_ascii = function '\000' .. '\127' -> true | _ -> false

exception Not_satisfy

type t =
  { normalized : string
  ; raw : string }

let pp ppf t =
  Fmt.pf ppf "{ @[<hov>normalized = %s;@ raw = @[<hov>%a@]@] }"
    t.normalized
    Utils.pp_string t.raw

let failf = Fmt.kstrf fail

let with_uutf is =
  let res = Buffer.create 16 in
  let tmp = Bytes.create 1 in
  let dec = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let not_satisfy = ref false in
  scan (Uutf.decode dec) (fun state chr ->
      let res = match state with
        | `Await -> Some state
        | `End -> None
        | `Uchar uchar ->
          Uutf.Buffer.add_utf_8 res uchar ; Some state
        | `Malformed _ ->
          Uutf.Buffer.add_utf_8 res Uutf.u_rep ; Some state in
      let res = match res with
        | Some state ->

          if (is_ascii chr && is chr) || not (is_ascii chr)
          then ( Bytes.unsafe_set tmp 0 chr
               ; Uutf.Manual.src dec tmp 0 1
               ; Some (Uutf.decode dec))
          else ( not_satisfy := true
               ; None )
        | None -> None in
      res)
  >>= fun (consumed, state) -> match !not_satisfy with
  | false ->
    (* assert (state = `End) ;
       TODO: assert false with [parse_string unstructured
             "p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC24+ \
             oWY3VEvkeJ8ZYCpp00YX61+Yyya6mgxgx6fbjUqAgaaqq \
             DdQoByt05XUYMKFV7Zs+wbtqNlJe15jUActNAu06LQnrM \
             Hhtdmepad/8jYR8YRhoPppKG6MmDlIRkzcmAA/E8ZZF7h \
             gaAiOvCtnoTY0/ZTabr3wC9NPpiL5tn1QIDAQAB" *)
    let normalized = Buffer.contents res in
    Buffer.clear res ;
    return { normalized; raw= consumed; }
  | true ->
    (* XXX(dinosaure): if we retrieve [not_satisfy = true], [state] was already
       computed by [scan]. We need to signal to [dec] end of input and compute
       returned and last state. *)
    Uutf.Manual.src dec Bytes.empty 0 0 ; (* `End *)
    match Uutf.decode dec with
    | `Await -> assert false
    | `End ->
      let normalized = Buffer.contents res in
      Buffer.clear res ;
      return { normalized; raw= consumed; }
    | `Uchar uchar ->
      assert (Uutf.decode dec = `End) ;
      Uutf.Buffer.add_utf_8 res uchar ;
      let normalized = Buffer.contents res in
      Buffer.clear res ;
      return { normalized; raw= consumed; }
    | `Malformed _ ->
      assert (Uutf.decode dec = `End) ;
      Uutf.Buffer.add_utf_8 res Uutf.u_rep ;
      let normalized = Buffer.contents res in
      Buffer.clear res ;
      return { normalized; raw= consumed; }

(*
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
*)

let with_uutf_without_raw is =
  with_uutf is >>| fun { normalized; _ } -> normalized

let with_uutf1 is =
  with_uutf is
  >>= fun r ->
  if String.length r.raw > 0
  then return r
  else failf "with_uutf1: string is empty @[<hov>%a@]" pp r

let with_uutf1_without_raw is =
  with_uutf1 is >>| fun { normalized; _ } -> normalized
