open Crowbar

let day =
  choose
    [ const Mrmime.Date.Day.Mon;
      const Mrmime.Date.Day.Tue;
      const Mrmime.Date.Day.Wed;
      const Mrmime.Date.Day.Thu;
      const Mrmime.Date.Day.Fri;
      const Mrmime.Date.Day.Sat;
      const Mrmime.Date.Day.Sun
    ]

let month =
  choose
    [ const Mrmime.Date.Month.Jan;
      const Mrmime.Date.Month.Feb;
      const Mrmime.Date.Month.Mar;
      const Mrmime.Date.Month.Apr;
      const Mrmime.Date.Month.May;
      const Mrmime.Date.Month.Jun;
      const Mrmime.Date.Month.Jul;
      const Mrmime.Date.Month.Aug;
      const Mrmime.Date.Month.Sep;
      const Mrmime.Date.Month.Oct;
      const Mrmime.Date.Month.Nov;
      const Mrmime.Date.Month.Dec
    ]

let military_zone =
  map
    [ range 25 ]
    (fun n ->
      match Char.unsafe_chr n with
      | '\000' .. '\008' ->
          Mrmime.Date.Zone.Military_zone (Char.unsafe_chr (n + 65))
      | '\009' .. '\024' ->
          Mrmime.Date.Zone.Military_zone (Char.unsafe_chr (n + 1 + 65))
      | _ -> assert false)

let tz = map [ range 24; range 60 ] (fun a b -> Mrmime.Date.Zone.TZ (a, b))

let zone =
  choose
    [ const Mrmime.Date.Zone.UT;
      const Mrmime.Date.Zone.GMT;
      const Mrmime.Date.Zone.EST;
      const Mrmime.Date.Zone.EDT;
      const Mrmime.Date.Zone.CST;
      const Mrmime.Date.Zone.CDT;
      const Mrmime.Date.Zone.MST;
      const Mrmime.Date.Zone.MDT;
      const Mrmime.Date.Zone.PST;
      const Mrmime.Date.Zone.PDT;
      military_zone;
      tz
    ]

let year = choose [ range ~min:1990 3000; range ~min:90 100 ]
let hours = range 25
let minutes = range 61
let seconds = range 61

let date =
  map
    [ option day; range 31; month; year; hours; minutes; option seconds; zone ]
    (fun day date month year hours minutes seconds zone ->
      { Mrmime.Date.day;
        date = (date, month, year);
        time = (hours, minutes, seconds);
        zone
      })

module BBuffer = Buffer

let emitter_of_buffer buf =
  let open Prettym in
  let write a = function
    | { IOVec.buffer = Buffer.String x; off; len } ->
        BBuffer.add_substring buf x off len;
        a + len
    | { IOVec.buffer = Buffer.Bytes x; off; len } ->
        BBuffer.add_subbytes buf x off len;
        a + len
    | { IOVec.buffer = Buffer.Bigstring x; off; len } ->
        BBuffer.add_string buf (Bigstringaf.substring x ~off ~len);
        a + len
  in
  List.fold_left write 0

let ( <.> ) f g x = f (g x)

let parser buf =
  let open Angstrom in
  Unstrctrd_parser.unstrctrd buf >>= fun v ->
  let res =
    let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
    Unstrctrd.without_comments v
    >>| Unstrctrd.fold_fws
    >>| Unstrctrd.to_utf_8_string
    >>= (Result.map_error (fun x -> `Msg x)
        <.> Angstrom.parse_string ~consume:Prefix Mrmime.Date.Decoder.date_time
        )
  in
  match res with Ok v -> return v | Error (`Msg err) -> fail err

let () =
  let open Mrmime in
  Crowbar.add_test ~name:"date" [ date ] @@ fun date ->
  let buffer = Buffer.create 0x100 in
  let encoder =
    Prettym.create ~margin:78 ~new_line:"\r\n" 0x100
      ~emitter:(emitter_of_buffer buffer)
  in
  let encoder =
    Prettym.keval Prettym.flush encoder
      Prettym.[ !!Date.Encoder.date; new_line ]
      date
  in

  check_eq ~pp:Fmt.bool ~eq:( = ) (Prettym.is_empty encoder) true;

  let result = Buffer.contents buffer in
  let buf = Bytes.create 0x7f in

  match Angstrom.parse_string ~consume:Prefix (parser buf) result with
  | Ok date' -> check_eq ~pp:Date.pp ~eq:Date.equal date date'
  | Error err -> failf "%a can not be parsed: %s" Date.pp date err

let ( <.> ) f g x = f (g x)

let () =
  let open Mrmime in
  Crowbar.add_test ~name:"date & ptime" [ float; zone ] @@ fun seconds zone ->
  (* XXX(dinosaure): according [Ptime]'s documentation, subsecond precision are
     floored. *)
  match Ptime.of_float_s (Stdlib.Float.floor seconds) with
  | None -> Crowbar.bad_test ()
  | Some ptime -> (
      let date = Date.of_ptime ~zone ptime in
      match Date.to_ptime date with
      | Ok (ptime', _tz_offset_s) ->
          check_eq ~pp:Fmt.int64 ~eq:Int64.equal
            ((Int64.of_float <.> Ptime.to_float_s) ptime)
            ((Int64.of_float <.> Ptime.to_float_s) ptime')
      | Error (`Msg err) ->
          failf "isormisphm was not respected on %a: %s" Mrmime.Date.pp date err
      )
