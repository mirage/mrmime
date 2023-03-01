let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

module Day = struct
  type t = Mon | Tue | Wed | Thu | Fri | Sat | Sun

  let mon = Mon
  let tue = Tue
  let wed = Wed
  let thu = Thu
  let fri = Fri
  let sat = Sat
  let sun = Sun

  let pp ppf = function
    | Mon -> Format.pp_print_string ppf "Mon"
    | Tue -> Format.pp_print_string ppf "Tue"
    | Wed -> Format.pp_print_string ppf "Wed"
    | Thu -> Format.pp_print_string ppf "Thu"
    | Fri -> Format.pp_print_string ppf "Fri"
    | Sat -> Format.pp_print_string ppf "Sat"
    | Sun -> Format.pp_print_string ppf "Sun"

  let to_string = Format.asprintf "%a" pp

  let of_string = function
    | "Mon" -> Ok Mon
    | "Tue" -> Ok Tue
    | "Wed" -> Ok Wed
    | "Thu" -> Ok Thu
    | "Fri" -> Ok Fri
    | "Sat" -> Ok Sat
    | "Sun" -> Ok Sun
    | x -> error_msgf "invalid day %s" x

  let of_string_exn x =
    match of_string x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg "%s" err

  let v x = of_string_exn x

  let equal a b =
    match (a, b) with
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
  type t =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec

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

  let equal a b =
    match (a, b) with
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
    | Jan -> Format.pp_print_string ppf "Jan"
    | Feb -> Format.pp_print_string ppf "Feb"
    | Mar -> Format.pp_print_string ppf "Mar"
    | Apr -> Format.pp_print_string ppf "Apr"
    | May -> Format.pp_print_string ppf "May"
    | Jun -> Format.pp_print_string ppf "Jun"
    | Jul -> Format.pp_print_string ppf "Jul"
    | Aug -> Format.pp_print_string ppf "Aug"
    | Sep -> Format.pp_print_string ppf "Sep"
    | Oct -> Format.pp_print_string ppf "Oct"
    | Nov -> Format.pp_print_string ppf "Nov"
    | Dec -> Format.pp_print_string ppf "Dec"

  let to_int = function
    | Jan -> 1
    | Feb -> 2
    | Mar -> 3
    | Apr -> 4
    | May -> 5
    | Jun -> 6
    | Jul -> 7
    | Aug -> 8
    | Sep -> 9
    | Oct -> 10
    | Nov -> 11
    | Dec -> 12

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
    | n -> error_msgf "Invalid number of month: %d" n

  let of_int_exn x =
    match of_int x with Ok v -> v | Error (`Msg err) -> invalid_arg "%s" err

  let to_string = Format.asprintf "%a" pp

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
    | x -> error_msgf "Invalid month %s" x

  let of_string_exn x =
    match of_string x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg "%s" err

  let v x = of_string_exn x
end

module Zone = struct
  type t =
    | UT
    | GMT
    | EST
    | EDT
    | CST
    | CDT
    | MST
    | MDT
    | PST
    | PDT
    | Military_zone of char
    | TZ of int * int

  let equal a b =
    match (a, b) with
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

  let is_military_zone = function
    | '\065' .. '\073' | '\075' .. '\090' | '\097' .. '\105' | '\107' .. '\122'
      ->
        true
    | _ -> false

  let military_zone = function
    | ('A' .. 'I' | 'K' .. 'Z') as chr -> Ok (Military_zone chr)
    | ('a' .. 'i' | 'k' .. 'z') as chr ->
        let chr = Char.chr (Char.code chr - 32) in
        Ok (Military_zone chr)
    | chr -> error_msgf "Invalid military zone '%c'" chr

  let tz hh mm =
    if abs hh >= 0 && abs hh < 24 && mm >= 0 && mm < 60 then Ok (TZ (hh, mm))
    else error_msgf "Invalid time-zone (hours: %d, minutes: %d)" hh mm

  let pp ppf = function
    | UT -> Format.pp_print_string ppf "UT"
    | GMT -> Format.pp_print_string ppf "GMT"
    | EST -> Format.pp_print_string ppf "EST"
    | EDT -> Format.pp_print_string ppf "EDT"
    | CST -> Format.pp_print_string ppf "CST"
    | CDT -> Format.pp_print_string ppf "CDT"
    | MST -> Format.pp_print_string ppf "MST"
    | MDT -> Format.pp_print_string ppf "MDT"
    | PST -> Format.pp_print_string ppf "PST"
    | PDT -> Format.pp_print_string ppf "PDT"
    | TZ (hh, mm) -> Format.fprintf ppf "(TZ %02d%02d)" hh mm
    | Military_zone c -> Format.fprintf ppf "(Military_zone %c)" c

  let to_string = function
    | TZ (hh, mm) ->
        if hh >= 0 then Format.asprintf "+%02d%02d" hh mm
        else Format.asprintf "-%02d%02d" (abs hh) mm
    | Military_zone c -> String.make 1 c
    | x -> Format.asprintf "%a" pp x

  let to_int = function
    | UT | GMT -> (00, 00)
    | EST -> (-05, 00)
    | EDT -> (-04, 00)
    | CST -> (-06, 00)
    | CDT -> (-05, 00)
    | MST -> (-07, 00)
    | MDT -> (-06, 00)
    | PST -> (-08, 00)
    | PDT -> (-07, 00)
    | TZ (hh, mm) -> (hh, mm)
    | Military_zone c -> (
        match c with
        | 'A' -> (01, 00)
        | 'B' -> (02, 00)
        | 'C' -> (03, 00)
        | 'D' -> (04, 00)
        | 'E' -> (05, 00)
        | 'F' -> (06, 00)
        | 'G' -> (07, 00)
        | 'H' -> (08, 00)
        | 'I' -> (09, 00)
        | 'K' -> (10, 00)
        | 'L' -> (11, 00)
        | 'M' -> (12, 00)
        | 'N' -> (-01, 00)
        | 'O' -> (-02, 00)
        | 'P' -> (-03, 00)
        | 'Q' -> (-04, 00)
        | 'R' -> (-05, 00)
        | 'S' -> (-06, 00)
        | 'T' -> (-07, 00)
        | 'U' -> (-08, 00)
        | 'V' -> (-09, 00)
        | 'W' -> (-10, 00)
        | 'X' -> (-11, 00)
        | 'Y' -> (-12, 00)
        | 'Z' -> (00, 00)
        | c -> invalid_arg "Invalid military zone %c" c)

  let parser_tz =
    let open Angstrom in
    let is_digit = function '0' .. '9' -> true | _ -> false in
    option '+' (satisfy (function '+' | '-' -> true | _ -> false))
    >>= fun sign ->
    satisfy is_digit >>= fun z0 ->
    satisfy is_digit >>= fun z1 ->
    satisfy is_digit >>= fun z2 ->
    satisfy is_digit >>= fun z3 ->
    let one =
      let res = Bytes.create 2 in
      Bytes.set res 0 z0;
      Bytes.set res 1 z1;
      Bytes.unsafe_to_string res
    in
    let two =
      let res = Bytes.create 2 in
      Bytes.set res 0 z2;
      Bytes.set res 1 z3;
      Bytes.unsafe_to_string res
    in
    let one = if sign = '-' then -int_of_string one else int_of_string one in
    let two = int_of_string two in
    if abs one >= 0 && abs one < 24 && two >= 0 && two < 60 then
      return (one, two)
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
    | x -> (
        match
          Angstrom.parse_string ~consume:Angstrom.Consume.All parser_tz x
        with
        | Ok (hh, mm) -> Ok (TZ (hh, mm))
        | Error _ ->
            if String.length x = 1 && is_military_zone x.[0] then
              Ok (Military_zone x.[0])
            else error_msgf "Invalid time-zone: %S" x)

  let of_string_exn x =
    match of_string x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg "%s" err

  let v x = of_string_exn x
end

type t =
  { day : Day.t option;
    date : int * Month.t * int;
    time : int * int * int option;
    zone : Zone.t
  }

let pp_ptime_day ppf x =
  let f = function
    | `Mon -> Day.Mon
    | `Thu -> Day.Thu
    | `Tue -> Day.Tue
    | `Wed -> Day.Wed
    | `Fri -> Day.Fri
    | `Sat -> Day.Sat
    | `Sun -> Day.Sun
  in
  Day.pp ppf (f x)

let make ?day (y, m, d) (hh, mm, ss) zone =
  let z =
    let hh, mm = Zone.to_int zone in
    (hh * 3600) + (mm * 60)
  in
  let same_day a ptime_b =
    match (a, ptime_b) with
    | Day.Mon, `Mon -> true
    | Day.Thu, `Thu -> true
    | Day.Tue, `Tue -> true
    | Day.Wed, `Wed -> true
    | Day.Fri, `Fri -> true
    | Day.Sat, `Sat -> true
    | Day.Sun, `Sun -> true
    | _, _ -> false
  in
  let m' = Month.to_int m in
  match
    Ptime.of_date_time ((y, m', d), ((hh, mm, Option.value ~default:0 ss), z))
  with
  | None -> error_msgf "Invalid date"
  | Some t -> (
      let day' = Ptime.weekday ~tz_offset_s:z t in

      match day with
      | None -> Ok { day = None; date = (d, m, y); time = (hh, mm, ss); zone }
      | Some day ->
          if same_day day day' then
            Ok { day = Some day; date = (d, m, y); time = (hh, mm, ss); zone }
          else
            error_msgf "Expected day mismatch (%a <> %a)" Day.pp day
              pp_ptime_day day')

let pp ppf = function
  | { day = Some day; date = d, m, y; time = hh, mm, ss; zone } ->
      Format.fprintf ppf
        "{@[<hov>day = %a;@ date = (@[<hov>%d,@ %a,@ %d@]);@ time = \
         (@[<hov>%d,@ %d,@ %d@]);@ zone = %a@]}"
        Day.pp day d Month.pp m y hh mm
        (Option.value ~default:0 ss)
        Zone.pp zone
  | { day = None; date = d, m, y; time = hh, mm, ss; zone } ->
      Format.fprintf ppf
        "{@[<hov>date = (@[<hov>%d,@ %a,@ %d@]);@ time = (@[<hov>%d,@ %d,@ \
         %d@]);@ zone = %a@]}"
        d Month.pp m y hh mm
        (Option.value ~default:0 ss)
        Zone.pp zone

let to_ptime date =
  let z =
    let hh, mm = Zone.to_int date.zone in
    (hh * 3600) + (mm * 60)
  in
  let m =
    let _, m, _ = date.date in
    Month.to_int m
  in
  let d, _, y = date.date in
  let hh, mm, ss = date.time in
  let ss = Option.value ~default:0 ss in
  match Ptime.of_date_time ((y, m, d), ((hh, mm, ss), z)) with
  | Some ptime -> Ok (ptime, z)
  | None -> error_msgf "Invalid date: %a" pp date

let of_ptime ~zone ptime =
  let tz_offset_s =
    let hh, mm = Zone.to_int zone in
    (hh * 3600) + (mm * 60)
  in
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s ptime in
  let date = (y, Month.of_int_exn m, d) in
  let day =
    match Ptime.weekday ~tz_offset_s ptime with
    | `Mon -> Day.Mon
    | `Tue -> Day.Tue
    | `Wed -> Day.Wed
    | `Thu -> Day.Thu
    | `Fri -> Day.Fri
    | `Sat -> Day.Sat
    | `Sun -> Day.Sun
  in
  match make ~day date (hh, mm, Some ss) zone with
  | Ok date_time -> date_time
  | Error _ -> assert false
(* XXX(dinosaure): error can reach if [Ptime.of_date_time] fails (but values
   come from [Ptime.to_date_time]), so this should not fail. Then, an other path
   is when we define a [?day] and it does not correspond to what [Ptime] expecs.
   Of course, this call, we did not notice [?day]. *)

let compare a b =
  match (to_ptime a, to_ptime b) with
  | Ok (a, _tz_a), Ok (b, _tz_b) -> Ptime.compare a b
  | Error (`Msg err), _ -> failwith err
  | _, Error (`Msg err) -> failwith err

let equal_option equal a b =
  match (a, b) with
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

module Decoder = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_wsp = function ' ' | '\t' -> true | _ -> false

  let two_digit =
    lift2
      (fun a b ->
        let res = Bytes.create 2 in
        Bytes.unsafe_set res 0 a;
        Bytes.unsafe_set res 1 b;
        Bytes.unsafe_to_string res)
      (satisfy is_digit) (satisfy is_digit)

  let four_digit =
    lift4
      (fun a b c d ->
        let res = Bytes.create 4 in
        Bytes.unsafe_set res 0 a;
        Bytes.unsafe_set res 1 b;
        Bytes.unsafe_set res 2 c;
        Bytes.unsafe_set res 3 d;
        Bytes.unsafe_to_string res)
      (satisfy is_digit) (satisfy is_digit) (satisfy is_digit)
      (satisfy is_digit)

  let at_least_n_digit n =
    take_while1 is_digit >>= fun res ->
    if String.length res >= n then return res else fail "at_least_n_digit"

  let one_or_two_digit =
    satisfy is_digit >>= fun one ->
    peek_char >>= function
    | Some two when is_digit two ->
        let res = Bytes.create 2 in
        Bytes.unsafe_set res 0 one;
        Bytes.unsafe_set res 1 two;
        advance 1 *> return (Bytes.unsafe_to_string res)
    | _ -> return (String.make 1 one)

  (* From RFC 2822

       obs-hour        =       [CFWS] 2DIGIT [CFWS]

     From RFC 5322

       obs-hour        =   [CFWS] 2DIGIT [CFWS]
  *)
  let obs_hour =
    skip_while is_wsp *> two_digit <* skip_while is_wsp >>| int_of_string

  (* From RFC 2822

       obs-minute      =       [CFWS] 2DIGIT [CFWS]

     From RFC 5322

       obs-minute      =   [CFWS] 2DIGIT [CFWS]
  *)
  let obs_minute =
    skip_while is_wsp *> two_digit <* skip_while is_wsp >>| int_of_string

  (* From RFC 2822

       obs-second      =       [CFWS] 2DIGIT [CFWS]

     From RFC 5322

       obs-second      =   [CFWS] 2DIGIT [CFWS]
  *)
  let obs_second =
    skip_while is_wsp *> two_digit <* skip_while is_wsp >>| int_of_string

  (* From RFC 2822

       hour            =       2DIGIT / obs-hour

     From RFC 5322

       hour            =   2DIGIT / obs-hour
  *)
  let hour = obs_hour <|> (two_digit >>| int_of_string)

  (* From RFC 2822

       minute          =       2DIGIT / obs-minute

     From RFC 5322

       minute          =   2DIGIT / obs-minute
  *)
  let minute = obs_minute <|> (two_digit >>| int_of_string)

  (* From RFC 2822

       second          =       2DIGIT / obs-second

     From RFC 5322

       second          =   2DIGIT / obs-second
  *)
  let second =
    obs_second <|> (two_digit >>| int_of_string) >>= fun res ->
    option "" (char '.' *> take_while1 is_digit) >>= fun _ns -> return res
  (* XXX(dinosaure): On [Received] field, the date can have nano-second. Such
   * value does not follow any standards but we must consume it to be able to
   * parse then zone value. It's an hot-fix to be able to accept several wrong
   * [Received] fields. *)

  (* From RFC 2822

       obs-year        =       [CFWS] 2*DIGIT [CFWS]

     From RFC 5322

       obs-year        =   [CFWS] 2*DIGIT [CFWS]
  *)
  let obs_year =
    skip_while is_wsp *> at_least_n_digit 2
    <* skip_while is_wsp
    >>| int_of_string

  (* From RFC 2822

       year            =       4*DIGIT / obs-year

       Where a two or three digit year occurs in a date, the year is to be
       interpreted as follows: If a two digit year is encountered whose
       value is between 00 and 49, the year is interpreted by adding 2000,
       ending up with a value between 2000 and 2049.  If a two digit year is
       encountered with a value between 50 and 99, or any three digit year
       is encountered, the year is interpreted by adding 1900.

       Differences from Earlier Specifications
       3.   Four or more digits allowed for year.
       15.  Two digit years not allowed.*
       16.  Three digit years interpreted, but not allowed for generation.*

     From RFC 5322

       year            =   (FWS 4*DIGIT FWS) / obs-year

       Where a two or three digit year occurs in a date, the year is to be
       interpreted as follows: If a two digit year is encountered whose
       value is between 00 and 49, the year is interpreted by adding 2000,
       ending up with a value between 2000 and 2049.  If a two digit year is
       encountered with a value between 50 and 99, or any three digit year
       is encountered, the year is interpreted by adding 1900.

       Differences from Earlier Specifications
       3.   Four or more digits allowed for year.
       15.  Two digit years not allowed.*
       16.  Three digit years interpreted, but not allowed for generation.*
  *)
  let year =
    skip_while is_wsp *> at_least_n_digit 4
    <* skip_while is_wsp
    >>| int_of_string
    <|> obs_year

  (* From RFC 2822

       obs-day         =       [CFWS] 1*2DIGIT [CFWS]

     From RFC 5322

       obs-day         =   [CFWS] 1*2DIGIT [CFWS]
  *)
  let obs_day =
    skip_while is_wsp *> one_or_two_digit <* skip_while is_wsp >>| int_of_string

  (* From RFC 2822

       day             =       ([FWS] 1*2DIGIT) / obs-day

     From RFC 5322

       day             =   ([FWS] 1*2DIGIT FWS) / obs-day
  *)
  let day =
    obs_day
    <|> (skip_while is_wsp *> one_or_two_digit
        <* skip_while is_wsp
        >>| int_of_string)

  (* From RFC 822

       month       =  "Jan"  /  "Feb" /  "Mar"  /  "Apr"
                   /  "May"  /  "Jun" /  "Jul"  /  "Aug"
                   /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"

     From RFC 2822

       month           =       (FWS month-name FWS) / obs-month

       month-name      =       "Jan" / "Feb" / "Mar" / "Apr" /
                               "May" / "Jun" / "Jul" / "Aug" /
                               "Sep" / "Oct" / "Nov" / "Dec"

     From RFC 5322

       month           =   "Jan" / "Feb" / "Mar" / "Apr" /
                           "May" / "Jun" / "Jul" / "Aug" /
                           "Sep" / "Oct" / "Nov" / "Dec"
  *)
  let month =
    string "Jan" *> return Month.Jan
    <|> string "Feb" *> return Month.Feb
    <|> string "Mar" *> return Month.Mar
    <|> string "Apr" *> return Month.Apr
    <|> string "May" *> return Month.May
    <|> string "Jun" *> return Month.Jun
    <|> string "Jul" *> return Month.Jul
    <|> string "Aug" *> return Month.Aug
    <|> string "Sep" *> return Month.Sep
    <|> string "Oct" *> return Month.Oct
    <|> string "Nov" *> return Month.Nov
    <|> string "Dec" *> return Month.Dec

  (* From RFC 822

       day         =  "Mon"  / "Tue" /  "Wed"  / "Thu"
                   /  "Fri"  / "Sat" /  "Sun"

     From RFC 2822

       day-name        =       "Mon" / "Tue" / "Wed" / "Thu" /
                               "Fri" / "Sat" / "Sun"

     From RFC 5322

       day-name        =   "Mon" / "Tue" / "Wed" / "Thu" /
                           "Fri" / "Sat" / "Sun"
  *)
  let day_name =
    string "Mon" *> return Day.Mon
    <|> string "Tue" *> return Day.Tue
    <|> string "Wed" *> return Day.Wed
    <|> string "Thu" *> return Day.Thu
    <|> string "Fri" *> return Day.Fri
    <|> string "Sat" *> return Day.Sat
    <|> string "Sun" *> return Day.Sun

  (* From RFC 2822

       obs-day-of-week =       [CFWS] day-name [CFWS]

     From RFC 5322

       obs-day-of-week =   [CFWS] day-name [CFWS]
  *)
  let obs_day_of_week = skip_while is_wsp *> day_name <* skip_while is_wsp

  (* From RFC 2822

       day-of-week     =       ([FWS] day-name) / obs-day-of-week

     From RFC 5322

       day-of-week     =   ([FWS] day-name) / obs-day-of-week
  *)
  let day_of_week = obs_day_of_week <|> skip_while is_wsp *> day_name

  (* From RFC 822

       date        =  1*2DIGIT month 2DIGIT        ; day month year
                                                   ;  e.g. 20 Jun 82

     From RFC 2822

       date            =       day month year

     From RFC 5322

       date            =   day month year
  *)
  let date =
    lift3
      (fun day month year -> (day, month, year))
      (day <?> "day") (month <?> "month") (year <?> "year")

  (* From RFC 822

       hour        =  2DIGIT ":" 2DIGIT [":" 2DIGIT]

     From RFC 2822

       time-of-day     =       hour ":" minute [ ":" second ]

     From RFC 5322

       time-of-day     =   hour ":" minute [ ":" second ]
  *)
  let time_of_day =
    hour <?> "hour" >>= fun hour ->
    char ':' *> minute <?> "minute" >>= fun minute ->
    option None
      ( skip_while is_wsp *> char ':' *> second <?> "second" >>| fun second ->
        Some second )
    >>| fun second -> (hour, minute, second)

  (* From RFC 822

       zone        =  "UT"  / "GMT"                ; Universal Time
                                                   ; North American : UT
                   /  "EST" / "EDT"                ;  Eastern:  - 5/ - 4
                   /  "CST" / "CDT"                ;  Central:  - 6/ - 5
                   /  "MST" / "MDT"                ;  Mountain: - 7/ - 6
                   /  "PST" / "PDT"                ;  Pacific:  - 8/ - 7
                   /  1ALPHA                       ; Military: Z = UT;
                                                   ;  A:-1; (J not used)
                                                   ;  M:-12; N:+1; Y:+12
                   / ( ("+" / "-") 4DIGIT )        ; Local differential
                                                   ;  hours+min. (HHMM)

       Time zone may be indicated in several ways.  "UT" is Univer-
       sal  Time  (formerly called "Greenwich Mean Time"); "GMT" is per-
       mitted as a reference to Universal Time.  The  military  standard
       uses  a  single  character for each zone.  "Z" is Universal Time.
       "A" indicates one hour earlier, and "M" indicates 12  hours  ear-
       lier;  "N"  is  one  hour  later, and "Y" is 12 hours later.  The
       letter "J" is not used.  The other remaining two forms are  taken
       from ANSI standard X3.51-1975.  One allows explicit indication of
       the amount of offset from UT; the other uses  common  3-character
       strings for indicating time zones in North America.

     From RFC 2822

       obs-zone        =       "UT" / "GMT" /          ; Universal Time
                               "EST" / "EDT" /         ; Eastern:  - 5/ - 4
                               "CST" / "CDT" /         ; Central:  - 6/ - 5
                               "MST" / "MDT" /         ; Mountain: - 7/ - 6
                               "PST" / "PDT" /         ; Pacific:  - 8/ - 7
                                                       ;
                               %d65-73 /               ; Military zones - "A"
                               %d75-90 /               ; through "I" and "K"
                               %d97-105 /              ; through "Z", both
                               %d107-122               ; upper and lower case

       In the obsolete time zone, "UT" and "GMT" are indications of
       "Universal Time" and "Greenwich Mean Time" respectively and are both
       semantically identical to "+0000".

       The remaining three character zones are the US time zones.  The first
       letter, "E", "C", "M", or "P" stands for "Eastern", "Central",
       "Mountain" and "Pacific".  The second letter is either "S" for
       "Standard" time, or "D" for "Daylight" (or summer) time.  Their
       interpretations are as follows:

       EDT is semantically equivalent to -0400
       EST is semantically equivalent to -0500
       CDT is semantically equivalent to -0500
       CST is semantically equivalent to -0600
       MDT is semantically equivalent to -0600
       MST is semantically equivalent to -0700
       PDT is semantically equivalent to -0700
       PST is semantically equivalent to -0800

       The 1 character military time zones were defined in a non-standard
       way in [RFC822] and are therefore unpredictable in their meaning.
       The original definitions of the military zones "A" through "I" are
       equivalent to "+0100" through "+0900" respectively; "K", "L", and "M"
       are equivalent to  "+1000", "+1100", and "+1200" respectively; "N"
       through "Y" are equivalent to "-0100" through "-1200" respectively;
       and "Z" is equivalent to "+0000".  However, because of the error in
       [RFC822], they SHOULD all be considered equivalent to "-0000" unless
       there is out-of-band information confirming their meaning.

       Other multi-character (usually between 3 and 5) alphabetic time zones
       have been used in Internet messages.  Any such time zone whose
       meaning is not known SHOULD be considered equivalent to "-0000"
       unless there is out-of-band information confirming their meaning.

     From RFC 5322

       obs-zone        =   "UT" / "GMT" /     ; Universal Time
                                              ; North American UT
                                              ; offsets
                           "EST" / "EDT" /    ; Eastern:  - 5/ - 4
                           "CST" / "CDT" /    ; Central:  - 6/ - 5
                           "MST" / "MDT" /    ; Mountain: - 7/ - 6
                           "PST" / "PDT" /    ; Pacific:  - 8/ - 7
                                              ;
                           %d65-73 /          ; Military zones - "A"
                           %d75-90 /          ; through "I" and "K"
                           %d97-105 /         ; through "Z", both
                           %d107-122          ; upper and lower case

       Where a two or three digit year occurs in a date, the year is to be
       interpreted as follows: If a two digit year is encountered whose
       value is between 00 and 49, the year is interpreted by adding 2000,
       ending up with a value between 2000 and 2049.  If a two digit year is
       encountered with a value between 50 and 99, or any three digit year
       is encountered, the year is interpreted by adding 1900.

       In the obsolete time zone, "UT" and "GMT" are indications of
       "Universal Time" and "Greenwich Mean Time", respectively, and are
       both semantically identical to "+0000".

       The remaining three character zones are the US time zones.  The first
       letter, "E", "C", "M", or "P" stands for "Eastern", "Central",
       "Mountain", and "Pacific".  The second letter is either "S" for
       "Standard" time, or "D" for "Daylight Savings" (or summer) time.
       Their interpretations are as follows:

          EDT is semantically equivalent to -0400
          EST is semantically equivalent to -0500
          CDT is semantically equivalent to -0500
          CST is semantically equivalent to -0600
          MDT is semantically equivalent to -0600
          MST is semantically equivalent to -0700
          PDT is semantically equivalent to -0700
          PST is semantically equivalent to -0800

         The 1 character military time zones were defined in a non-standard
         way in [RFC0822] and are therefore unpredictable in their meaning.
         The original definitions of the military zones "A" through "I" are
         equivalent to "+0100" through "+0900", respectively; "K", "L", and
         "M" are equivalent to "+1000", "+1100", and "+1200", respectively;
         "N" through "Y" are equivalent to "-0100" through "-1200".
         respectively; and "Z" is equivalent to "+0000".  However, because of
         the error in [RFC0822], they SHOULD all be considered equivalent to
         "-0000" unless there is out-of-band information confirming their
         meaning.

         Other multi-character (usually between 3 and 5) alphabetic time zones
         have been used in Internet messages.  Any such time zone whose
         meaning is not known SHOULD be considered equivalent to "-0000"
         unless there is out-of-band information confirming their meaning.
  *)
  let obs_zone =
    string "UT" *> return Zone.UT
    <|> string "GMT" *> return Zone.GMT
    <|> string "EST" *> return Zone.EST
    <|> string "EDT" *> return Zone.EDT
    <|> string "CST" *> return Zone.CST
    <|> string "CDT" *> return Zone.CDT
    <|> string "MST" *> return Zone.MST
    <|> string "MDT" *> return Zone.MDT
    <|> string "PST" *> return Zone.PST
    <|> string "PDT" *> return Zone.PDT
    <|> ( satisfy Zone.is_military_zone >>= fun z ->
          return (Zone.Military_zone z) )

  (* From RFC 2822

       zone            =       (( "+" / "-" ) 4DIGIT) / obs-zone

       The zone specifies the offset from Coordinated Universal Time (UTC,
       formerly referred to as "Greenwich Mean Time") that the date and
       time-of-day represent.  The "+" or "-" indicates whether the
       time-of-day is ahead of (i.e., east of) or behind (i.e., west of)
       Universal Time.  The first two digits indicate the number of hours
       difference from Universal Time, and the last two digits indicate the
       number of minutes difference from Universal Time.  (Hence, +hhmm
       means +(hh * 60 + mm) minutes, and -hhmm means -(hh * 60 + mm)
       minutes).  The form "+0000" SHOULD be used to indicate a time zone at
       Universal Time.  Though "-0000" also indicates Universal Time, it is
       used to indicate that the time was generated on a system that may be
       in a local time zone other than Universal Time and therefore
       indicates that the date-time contains no information about the local
       time zone.

     From RFC 5322

       zone            =   (FWS ( "+" / "-" ) 4DIGIT) / obs-zone

       The zone specifies the offset from Coordinated Universal Time (UTC,
       formerly referred to as "Greenwich Mean Time") that the date and
       time-of-day represent.  The "+" or "-" indicates whether the time-of-
       day is ahead of (i.e., east of) or behind (i.e., west of) Universal
       Time.  The first two digits indicate the number of hours difference
       from Universal Time, and the last two digits indicate the number of
       additional minutes difference from Universal Time.  (Hence, +hhmm
       means +(hh * 60 + mm) minutes, and -hhmm means -(hh * 60 + mm)
       minutes).  The form "+0000" SHOULD be used to indicate a time zone at
       Universal Time.  Though "-0000" also indicates Universal Time, it is
       used to indicate that the time was generated on a system that may be
       in a local time zone other than Universal Time and that the date-time
       contains no information about the local time zone.
  *)
  let zone =
    (* XXX(dinosaure): we clearly have a bug in this place. Indeed, ABNF expects
       an explicit space between [zone] and [time_of_day]. However, if we see
       [second] or [minute], they are surrounded by [CFWS]. That mean, they
       consume trailing spaces. If we explicitly expect [FWS] here, we will fail -
       mostly because this expected space is a part of [minute] or [second]. To
       avoid an error, [FWS] is optional but a better way should to check if we
       consumed at least one space before [zone]. *)
    skip_while is_wsp *> satisfy (function '+' | '-' -> true | _ -> false)
    <?> "sign"
    >>= (fun sign ->
          four_digit <?> "four-digit" >>| fun zone ->
          let one =
            if sign = '-' then -int_of_string (String.sub zone 0 2)
            else int_of_string (String.sub zone 0 2)
          in
          let two = int_of_string (String.sub zone 2 2) in
          Zone.TZ (one, two))
    <|> skip_while is_wsp *> obs_zone

  (* From RFC 822

       time        =  hour zone                    ; ANSI and Military

     From RFC 2822

       time            =       time-of-day FWS zone

     From RFC 5322

       time            =   time-of-day zone
  *)
  let time =
    lift2
      (fun time zone -> (time, zone))
      (time_of_day <?> "time-of-day")
      (zone <?> "zone")

  (* From RFC 822

       date-time   =  [ day "," ] date time        ; dd mm yy
                                                   ;  hh:mm:ss zzz

     From RFC 2822

       Date and time occur in several header fields.  This section specifies
       the syntax for a full date and time specification.  Though folding
       white space is permitted throughout the date-time specification, it
       is RECOMMENDED that a single space be used in each place that FWS
       appears (whether it is required or optional); some older
       implementations may not interpret other occurrences of folding white
       space correctly.

       date-time       =       [ day-of-week "," ] date FWS time [CFWS]

     From RFC 5322

       Date and time values occur in several header fields.  This section
       specifies the syntax for a full date and time specification.  Though
       folding white space is permitted throughout the date-time
       specification, it is RECOMMENDED that a single space be used in each
       place that FWS appears (whether it is required or optional); some
       older implementations will not interpret longer sequences of folding
       white space correctly.

       date-time       =   [ day-of-week "," ] date time [CFWS]
  *)
  let date_time =
    lift3
      (fun day date (time, zone) -> { day; date; time; zone })
      (option None (day_of_week >>= fun day -> char ',' *> return (Some day)))
      date time
    <* skip_while is_wsp
end

module Encoder = struct
  open Prettym

  let day ppf =
    let day = using Day.to_string string in
    eval ppf [ !!day; char $ ','; fws ]

  let month = using Month.to_string string

  let time ppf (hours, minutes, seconds) =
    let string_of_number = Format.asprintf "%02d" in
    let number ppf x =
      eval ppf [ cut; !!(using string_of_number string); cut ] x
    in
    match seconds with
    | Some seconds ->
        eval ppf
          [ tbox 1;
            !!number;
            char $ ':';
            !!number;
            char $ ':';
            !!number;
            close
          ]
          hours minutes seconds
    | None ->
        eval ppf [ tbox 1; !!number; char $ ':'; !!number; close ] hours minutes

  let zone = using Zone.to_string string
  let int = using string_of_int string

  let date ppf t =
    let d, m, y = t.date in
    eval ppf
      [ tbox 1;
        !!(option day);
        !!int;
        fws;
        !!month;
        fws;
        !!int;
        fws;
        !!time;
        fws;
        !!zone;
        close
      ]
      t.day d m y t.time t.zone
end
