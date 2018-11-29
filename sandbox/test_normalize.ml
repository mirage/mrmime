#require "uutf" ;;
#require "rosetta" ;;
#require "pecu" ;;
#require "rresult" ;;

type share = [`End | `Uchar of Uchar.t ]

let normalize_quoted_printable ?(chunk = 512) ~charset raw =
  let tmp = Bytes.create chunk in
  let res = Buffer.create chunk in
  let pos = ref 0 in
  let qp_decoder = Pecu.Inline.decoder (`String raw) in
  let decoder = Rosetta.decoder charset `Manual in
  let encoder = Uutf.encoder `UTF_8 (`Buffer res) in
  let rec go_encode v =
    match v, Uutf.encode encoder v with
    | `End, `Ok -> `Ok
    | _, `Ok -> go_decode ()
    | _, `Partial -> assert false (* XXX(dinosaure): [Uutf.encoder_dst encoder <> `Manual] *)
  and go_decode () =
    match Rosetta.decode decoder with
    | `Await -> go_qp_decode ()
    | #share as v -> go_encode v
    | `Malformed _ as v -> v
  and go_qp_decode () =
    match Pecu.Inline.decode qp_decoder with
    | `Await ->
        assert false
        (* XXX(dinosaure): [Pecu.Inline.decoder_src qp_decoder <> `Manual]. *)
    | `Char chr ->
        Bytes.unsafe_set tmp !pos chr ;
        if !pos + 1 = chunk then ( pos := 0; Rosetta.src decoder tmp 0 chunk ) else incr pos ;
        go_decode ()
    | `End ->
        ( let i = !pos in pos := 0; Rosetta.src decoder tmp 0 i ) ;
        go_decode ()
    | `Malformed _ as v -> v
  in
  match go_qp_decode () with
  | `Ok -> Ok (Buffer.contents res)
  | `Malformed data -> Rresult.R.error_msgf "Malformed input: %S" data

let a = normalize_quoted_printable ~charset:`ISO_8859_1 "Keld_J=F8rn_Simonsen"
let b = normalize_quoted_printable ~charset:`ISO_8859_1 "Andr=E9"
let c = normalize_quoted_printable ~charset:`ISO_8859_1 "Olle_J=E4rnefors"
let d = normalize_quoted_printable ~charset:`ISO_8859_1 "Patrick_F=E4ltstr=F6m"
