open Crowbar

let day =
  choose
    [ const Mrmime.Date.Day.Mon
    ; const Mrmime.Date.Day.Tue
    ; const Mrmime.Date.Day.Wed
    ; const Mrmime.Date.Day.Thu
    ; const Mrmime.Date.Day.Fri
    ; const Mrmime.Date.Day.Sat
    ; const Mrmime.Date.Day.Sun ]

let month =
  choose
    [ const Mrmime.Date.Month.Jan
    ; const Mrmime.Date.Month.Feb
    ; const Mrmime.Date.Month.Mar
    ; const Mrmime.Date.Month.Apr
    ; const Mrmime.Date.Month.May
    ; const Mrmime.Date.Month.Jun
    ; const Mrmime.Date.Month.Jul
    ; const Mrmime.Date.Month.Aug
    ; const Mrmime.Date.Month.Sep
    ; const Mrmime.Date.Month.Oct
    ; const Mrmime.Date.Month.Nov
    ; const Mrmime.Date.Month.Dec ]

let military_zone =
  map [ range 25 ]
    (fun n -> match Char.unsafe_chr n with
       | '\000' .. '\008' -> Mrmime.Date.Zone.Military_zone (Char.unsafe_chr (n + 65))
       | '\009' .. '\024' -> Mrmime.Date.Zone.Military_zone (Char.unsafe_chr (n + 1 + 65))
       | _ -> assert false)

let tz =
  map [ range 100; range 100 ] (fun a b -> Mrmime.Date.Zone.TZ (a, b))

let zone =
  choose
    [ const Mrmime.Date.Zone.UT
    ; const Mrmime.Date.Zone.GMT
    ; const Mrmime.Date.Zone.EST
    ; const Mrmime.Date.Zone.EDT
    ; const Mrmime.Date.Zone.CST
    ; const Mrmime.Date.Zone.CDT
    ; const Mrmime.Date.Zone.MST
    ; const Mrmime.Date.Zone.MDT
    ; const Mrmime.Date.Zone.PST
    ; const Mrmime.Date.Zone.PDT
    ; military_zone
    ; tz ]

let year =
  choose
    [ range ~min:1990 3000
    ; range ~min:90 100 ]

let hours = range 25
let minutes = range 61
let seconds = range 61

let date =
  map [ option day; range 31; month; year; hours; minutes; option seconds; zone ]
    (fun day date month year hours minutes seconds zone ->
       { Mrmime.Date.day
       ; date= (date, month, year)
       ; time= (hours, minutes, seconds)
       ; zone })

let writer_of_buffer buf =
  let open Mrmime.Encoder in

  let write a = function
    | { Level0.IOVec.buffer= Level0.Buffer.String x; off; len; } ->
      Buffer.add_substring buf x off len; a + len
    | { Level0.IOVec.buffer= Level0.Buffer.Bytes x; off; len; } ->
      Buffer.add_subbytes buf x off len; a + len
    | { Level0.IOVec.buffer= Level0.Buffer.Bigstring x; off; len; } ->
      Buffer.add_string buf (Bigstringaf.substring x ~off ~len); a + len in
  List.fold_left write 0

let () =
  let open Mrmime in

  Crowbar.add_test ~name:"date" [ date ] @@ fun date ->

  let buffer = Buffer.create 0x100 in
  let encoder = Encoder.Level1.create ~margin:78 ~new_line:"\r\n" 0x100 in
  let encoder = Encoder.with_writer encoder (writer_of_buffer buffer) in
  let _ = Encoder.eval encoder Encoder.[ !!Date.Encoder.date; new_line; new_line ] date in
  let result = Buffer.contents buffer in

  match Angstrom.parse_string Angstrom.(Rfc5322.date_time <* Rfc822.crlf <* Rfc822.crlf) result with
  | Ok date' ->
    check_eq ~pp:Date.pp ~eq:Date.equal date date'
  | Error err ->
    failf "%a can not be parsed: %s" Date.pp date err
