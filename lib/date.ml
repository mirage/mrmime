module Day = struct
  type t = Rfc5322.day =
    | Mon | Tue | Wed
    | Thu | Fri | Sat
    | Sun

  let mon = Mon
  let tue = Tue
  let wed = Wed
  let thu = Thu
  let fri = Fri
  let sat = Sat
  let sun = Sun

  let pp ppf = function
    | Mon  -> Fmt.pf ppf "Mon"
    | Tue  -> Fmt.pf ppf "Tue"
    | Wed  -> Fmt.pf ppf "Wed"
    | Thu  -> Fmt.pf ppf "Thu"
    | Fri  -> Fmt.pf ppf "Fri"
    | Sat  -> Fmt.pf ppf "Sat"
    | Sun  -> Fmt.pf ppf "Sun"

  let to_string = Fmt.to_to_string pp

  let of_string = function
    | "Mon" -> Ok Mon
    | "Tue" -> Ok Tue
    | "Wed" -> Ok Wed
    | "Thu" -> Ok Thu
    | "Fri" -> Ok Fri
    | "Sat" -> Ok Sat
    | "Sun" -> Ok Sun
    | x -> Rresult.R.error_msgf "invalid day %s" x

  let of_string_exn x = match of_string x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let v x = of_string_exn x

  let equal a b = match a, b with
    | Mon, Mon -> true
    | Tue, Tue -> true
    | Wed, Wed -> true
    | Thu, Thu -> true
    | Fri, Fri -> true
    | Sat, Sat -> true
    | Sun, Sun -> true
    | _, _ -> false
end

module Month = struct
  type t = Rfc5322.month =
    | Jan | Feb | Mar | Apr | May | Jun
    | Jul | Aug | Sep | Oct | Nov | Dec

  let jan = Jan
  let feb = Feb
  let mar = Mar
  let apr = Apr
  let may = May
  let jun = Jun
  let jul = Jul
  let aug = Aug
  let sep = Sep
  let oct = Oct
  let nov = Nov
  let dec = Dec

  let equal a b = match a, b with
    | Jan, Jan -> true
    | Feb, Feb -> true
    | Mar, Mar -> true
    | Apr, Apr -> true
    | May, May -> true
    | Jun, Jun -> true
    | Jul, Jul -> true
    | Aug, Aug -> true
    | Sep, Sep -> true
    | Oct, Oct -> true
    | Nov, Nov -> true
    | Dec, Dec -> true
    | _, _ -> false

  let pp ppf = function
    | Jan  -> Fmt.pf ppf "Jan"
    | Feb  -> Fmt.pf ppf "Feb"
    | Mar  -> Fmt.pf ppf "Mar"
    | Apr  -> Fmt.pf ppf "Apr"
    | May  -> Fmt.pf ppf "May"
    | Jun  -> Fmt.pf ppf "Jun"
    | Jul  -> Fmt.pf ppf "Jul"
    | Aug  -> Fmt.pf ppf "Aug"
    | Sep  -> Fmt.pf ppf "Sep"
    | Oct  -> Fmt.pf ppf "Oct"
    | Nov  -> Fmt.pf ppf "Nov"
    | Dec  -> Fmt.pf ppf "Dec"

  let to_int = function
    | Jan -> 1 | Feb -> 2
    | Mar -> 3 | Apr -> 4
    | May -> 5 | Jun -> 6
    | Jul -> 7 | Aug -> 8
    | Sep -> 9 | Oct -> 10
    | Nov -> 11 | Dec -> 12

  let of_int = function
    | 1 -> Ok Jan
    | 2 -> Ok Feb
    | 3 -> Ok Mar
    | 4 -> Ok Apr
    | 5 -> Ok May
    | 6 -> Ok Jun
    | 7 -> Ok Jul
    | 8 -> Ok Aug
    | 9 -> Ok Sep
    | 10 -> Ok Oct
    | 11 -> Ok Nov
    | 12 -> Ok Dec
    | n -> Rresult.R.error_msgf "Invalid number of month: %d" n

  let of_int_exn x = match of_int x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let to_string = Fmt.to_to_string pp

  let of_string = function
    | "Jan" -> Ok Jan
    | "Feb" -> Ok Feb
    | "Mar" -> Ok Mar
    | "Apr" -> Ok Apr
    | "May" -> Ok May
    | "Jun" -> Ok Jun
    | "Jul" -> Ok Jul
    | "Aug" -> Ok Aug
    | "Sep" -> Ok Sep
    | "Oct" -> Ok Oct
    | "Nov" -> Ok Nov
    | "Dec" -> Ok Dec
    | x -> Rresult.R.error_msgf "Invalid month %s" x

  let of_string_exn x = match of_string x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let v x = of_string_exn x
end

module Zone = struct
  type t = Rfc5322.zone =
    | UT  | GMT
    | EST | EDT
    | CST | CDT
    | MST | MDT
    | PST | PDT
    | Military_zone of char
    | TZ of int * int

  let equal a b = match a, b with
    | UT, UT -> true
    | GMT, GMT -> true
    | EST, EST -> true
    | EDT, EDT -> true
    | CST, CST -> true
    | CDT, CDT -> true
    | MST, MST -> true
    | MDT, MDT -> true
    | PST, PST -> true
    | PDT, PDT -> true
    | Military_zone a, Military_zone b -> Char.equal a b
    | TZ (aa, ab), TZ (ba, bb) -> aa = ba && ab = bb
    | _, _ -> false

  let ut = UT
  let gmt = GMT
  let est = EST
  let edt = EDT
  let cst = CST
  let cdt = CDT
  let mst = MST
  let mdt = MDT
  let pst = PST
  let pdt = PDT

  let military_zone = function
    | ('A' .. 'I' | 'K' .. 'Z') as chr -> Ok (Military_zone chr)
    | ('a' .. 'i' | 'k' .. 'z') as chr ->
      let chr = Char.chr (Char.code chr - 32) in
      Ok (Military_zone chr)
    | chr -> Rresult.R.error_msgf "Invalid military zone '%c'" chr

  let tz hh mm =
    if (abs hh) >= 0 && (abs hh) < 24
       && mm >= 0 && mm < 60
    then Ok (TZ (hh, mm))
    else Rresult.R.error_msgf "Invalid time-zone (hours: %d, minutes: %d)" hh mm

  let pp ppf = function
    | UT   -> Fmt.pf ppf "UT"
    | GMT  -> Fmt.pf ppf "GMT"
    | EST  -> Fmt.pf ppf "EST"
    | EDT  -> Fmt.pf ppf "EDT"
    | CST  -> Fmt.pf ppf "CST"
    | CDT  -> Fmt.pf ppf "CDT"
    | MST  -> Fmt.pf ppf "MST"
    | MDT  -> Fmt.pf ppf "MDT"
    | PST  -> Fmt.pf ppf "PST"
    | PDT  -> Fmt.pf ppf "PDT"
    | TZ (hh, mm) -> Fmt.pf ppf "(TZ %02d%02d)" hh mm
    | Military_zone c -> Fmt.pf ppf "(Military_zone %c)" c

  let to_string = function
    | TZ (hh, mm) -> if hh >= 0 then Fmt.strf "+%02d%02d" hh mm else Fmt.strf "-%02d%02d" (abs hh) mm
    | Military_zone c -> String.make 1 c
    | x -> Fmt.to_to_string pp x

  let to_int = function
    | UT | GMT -> 00, 00
    | EST -> -05, 00
    | EDT -> -04, 00
    | CST -> -06, 00
    | CDT -> -05, 00
    | MST -> -07, 00
    | MDT -> -06, 00
    | PST -> -08, 00
    | PDT -> -07, 00
    | TZ (hh, mm) -> hh, mm
    | Military_zone c -> match c with
      | 'A' -> 01, 00
      | 'B' -> 02, 00
      | 'C' -> 03, 00
      | 'D' -> 04, 00
      | 'E' -> 05, 00
      | 'F' -> 06, 00
      | 'G' -> 07, 00
      | 'H' -> 08, 00
      | 'I' -> 09, 00
      | 'K' -> 10, 00
      | 'L' -> 11, 00
      | 'M' -> 12, 00
      | 'N' -> -01, 00
      | 'O' -> -02, 00
      | 'P' -> -03, 00
      | 'Q' -> -04, 00
      | 'R' -> -05, 00
      | 'S' -> -06, 00
      | 'T' -> -07, 00
      | 'U' -> -08, 00
      | 'V' -> -09, 00
      | 'W' -> -10, 00
      | 'X' -> -11, 00
      | 'Y' -> -12, 00
      | 'Z' -> 00, 00
      | c -> Fmt.invalid_arg "Invalid military zone %c" c

  let parser_tz =
    let open Angstrom in
    let is_digit = function '0' .. '9' -> true | _ -> false in
    option '+' (satisfy (function '+' | '-' -> true | _ -> false))
    >>= fun sign -> satisfy is_digit
    >>= fun z0 -> satisfy is_digit
    >>= fun z1 -> satisfy is_digit
    >>= fun z2 -> satisfy is_digit
    >>= fun z3 ->
    let one = let res = Bytes.create 2 in Bytes.set res 0 z0 ; Bytes.set res 1 z1 ; Bytes.unsafe_to_string res in
    let two = let res = Bytes.create 2 in Bytes.set res 0 z2 ; Bytes.set res 1 z3 ; Bytes.unsafe_to_string res in
    let one = if sign = '-' then - int_of_string one else int_of_string one in
    let two = int_of_string two in
    if (abs one) >= 0 && (abs one) < 24
       && two >= 0 && two < 60
    then return (one, two)
    else fail "Invalid time-zone"

  let of_string = function
    | "UT" -> Ok UT
    | "GMT" -> Ok GMT
    | "EST" -> Ok EST
    | "EDT" -> Ok EDT
    | "CST" -> Ok CST
    | "CDT" -> Ok CDT
    | "MST" -> Ok MST
    | "MDT" -> Ok MDT
    | "PST" -> Ok PST
    | "PDT" -> Ok PDT
    | x ->
      match Angstrom.parse_string parser_tz x with
      | Ok (hh, mm) -> Ok (TZ (hh, mm))
      | Error _ ->
        if String.length x = 1 && Rfc5322.is_military_zone x.[0]
        then Ok (Military_zone x.[0])
        else Rresult.R.error_msgf "Invalid time-zone: %S" x

  let of_string_exn x = match of_string x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let v x = of_string_exn x
end

type t = Rfc5322.date =
  { day  : Day.t option
  ; date : int * Month.t * int
  ; time : int * int * int option
  ; zone : Zone.t }

let pp_ptime_day =
  let f = function
    | `Mon -> Day.Mon | `Thu -> Day.Thu | `Tue -> Day.Tue
    | `Wed -> Day.Wed | `Fri -> Day.Fri | `Sat -> Day.Sat | `Sun -> Day.Sun in
  Fmt.using f Day.pp

let make ?day (y, m, d) (hh, mm, ss) zone =
  let z =
    let hh, mm = Zone.to_int zone in
    hh * 3600 + mm * 60 in
  let same_day a ptime_b = match a, ptime_b with
    | Day.Mon, `Mon -> true
    | Day.Thu, `Thu -> true
    | Day.Tue, `Tue -> true
    | Day.Wed, `Wed -> true
    | Day.Fri, `Fri -> true
    | Day.Sat, `Sat -> true
    | Day.Sun, `Sun -> true
    | _, _ -> false in
  let m' = Month.to_int m in
  match Ptime.of_date_time ((y, m', d), ((hh, mm, Option.value ~default:0 ss), z)) with
  | None -> Rresult.R.error_msgf "Invalid date"
  | Some t ->
    let day' = Ptime.weekday ~tz_offset_s:z t in

    match day with
    | None -> Ok { day; date= (d, m, y); time= (hh, mm, ss); zone }
    | Some day ->
      if same_day day day'
      then Ok { day= Some day; date= (d, m, y); time= (hh, mm, ss); zone }
      else Rresult.R.error_msgf "Expected day mismatch (%a <> %a)" Day.pp day pp_ptime_day day'

let pp ppf = function
  | { day = Some day; date = (d, m, y); time = (hh, mm, ss); zone; } ->
    Fmt.pf ppf "{@[<hov>day = %a;@ \
                        date = (@[<hov>%d,@ %a,@ %d@]);@ \
                        time = (@[<hov>%d,@ %d,@ %d@]);@ \
                        zone = %a@]}"
      Day.pp day d Month.pp m y hh mm (Option.value ~default:0 ss)
      Zone.pp zone
  | { day = None; date = (d, m, y); time = (hh, mm, ss); zone; } ->
    Fmt.pf ppf "{@[<hov>date = (@[<hov>%d,@ %a,@ %d@]);@ \
                        time = (@[<hov>%d,@ %d,@ %d@]);@ \
                        zone = %a@]}"
      d Month.pp m y hh mm (Option.value ~default:0 ss)
      Zone.pp zone

let to_ptime date =
  let z =
    let hh, mm = Zone.to_int date.zone in
    hh * 3600 + mm * 60 in
  let m =
    let (_, m, _) = date.date in
    Month.to_int m in
  let (d, _, y) = date.date in
  let (hh, mm, ss) = date.time in
  let ss = Option.value ~default:0 ss in
  match Ptime.of_date_time ((y, m, d), ((hh, mm, ss), z)) with
  | Some ptime -> Ok ptime
  | None -> Rresult.R.error_msgf "Invalid date: %a" pp date

let of_ptime ~zone ptime =
  let tz_offset_s =
    let (hh, mm) = Zone.to_int zone in
    hh * 3600 + mm * 60 in
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s ptime in
  let date = (y, (Month.of_int_exn m), d) in
  match make date (hh, mm, Some ss) zone with
  | Ok date_time -> date_time
  | Error _ -> assert false
(* XXX(dinosaure): error can reach if [Ptime.of_date_time] fails (but values
   come from [Ptime.to_date_time]), so this should not fail. Then, an other path
   is when we define a [?day] and it does not correspond to what [Ptime] expecs.
   Of course, this call, we did not notice [?day]. *)

let compare a b = match to_ptime a, to_ptime b with
  | Ok a, Ok b -> Ptime.compare a b
  | Error (`Msg err), _ -> failwith err
  | _, Error (`Msg err) -> failwith err

let equal_option equal a b = match a, b with
  | Some a, Some b -> equal a b
  | None, None -> true
  | _, _ -> false

let equal_int a b = a = b

let equal_date (a_day, a_month, a_year) (b_day, b_month, b_year) =
  a_day = b_day && Month.equal a_month b_month && a_year = b_year

let equal_time (a_hour, a_min, a_sec) (b_hour, b_min, b_sec) =
  a_hour = b_hour && a_min = b_min && equal_option equal_int a_sec b_sec

let equal a b =
  equal_option Day.equal a.day b.day
  && equal_date a.date b.date
  && equal_time a.time b.time
  && Zone.equal a.zone b.zone

module Encoder = struct
  include Encoder

  let day ppf =
    let day = using Day.to_string string in
    eval ppf [ !!day; char $ ','; fws ]

  let month = using Month.to_string string

  let time ppf (hours, minutes, seconds) =
    let string_of_number = Fmt.strf "%02d" in
    let number ppf x = eval ppf [ cut; !!(using string_of_number string); cut ] x in
    match seconds with
    | Some seconds ->
      eval ppf [ tbox 1; !!number; char $ ':'; !!number; char $ ':'; !!number; close ]
        hours minutes seconds
    | None ->
      eval ppf [ tbox 1; !!number; char $ ':'; !!number; close ]
        hours minutes

  let zone = using Zone.to_string string
  let int = using string_of_int string

  let date ppf t =
    let (d, m, y) = t.date in
    eval ppf [ tbox 1; !!(option day); !!int; fws; !!month; fws; !!int; fws; !!time; fws; !!zone; close ]
      t.day d m y t.time t.zone
end

let to_string x = Encoder.to_string Encoder.date x
let to_unstructured = Unstructured.to_unstructured ~field_name:Field_name.date Encoder.date
