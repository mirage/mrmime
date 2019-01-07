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
    | "Mon" -> Mon
    | "Tue" -> Tue
    | "Wed" -> Wed
    | "Thu" -> Thu
    | "Fri" -> Fri
    | "Sat" -> Sat
    | "Sun" -> Sun
    | x -> Fmt.invalid_arg "invalid day %s" x

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
    | 1 -> Some Jan
    | 2 -> Some Feb
    | 3 -> Some Mar
    | 4 -> Some Apr
    | 5 -> Some May
    | 6 -> Some Jun
    | 7 -> Some Jul
    | 8 -> Some Aug
    | 9 -> Some Sep
    | 10 -> Some Oct
    | 11 -> Some Nov
    | 12 -> Some Dec
    | _ -> None

  let to_string = Fmt.to_to_string pp
  let of_string = function
    | "Jan" -> Jan
    | "Feb" -> Feb
    | "Mar" -> Mar
    | "Apr" -> Apr
    | "May" -> May
    | "Jun" -> Jun
    | "Jul" -> Jul
    | "Aug" -> Aug
    | "Sep" -> Sep
    | "Oct" -> Oct
    | "Nov" -> Nov
    | "Dec" -> Dec
    | x -> Fmt.invalid_arg "invalid month %s" x
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
    | ('A' .. 'I' | 'K' .. 'Z') as chr -> Some (Military_zone chr)
    | ('a' .. 'i' | 'k' .. 'z') as chr ->
      let chr = Char.chr (Char.code chr - 32) in
      Some (Military_zone chr)
    | _ -> None

  let tz x y = Some (TZ (x, y)) (* TODO *)

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
    | TZ (x, y) -> Fmt.pf ppf "(TZ %02d%02d)" x y
    | Military_zone c -> Fmt.pf ppf "(Military_zone %c)" c

  let to_string = function
    | TZ (x, y) -> Fmt.strf "%02d%02d" x y
    | Military_zone c -> String.make 1 c
    | x -> Fmt.to_to_string pp x

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
    return (one, two)

  let of_string = function
    | "UT" -> UT
    | "GMT" -> GMT
    | "EST" -> EST
    | "EDT" -> EDT
    | "CST" -> CST
    | "CDT" -> CDT
    | "MST" -> MST
    | "MDT" -> MDT
    | "PST" -> PST
    | "PDT" -> PDT
    | x ->
      match Angstrom.parse_string parser_tz x with
      | Ok (x, y) -> TZ (x, y)
      | Error _ ->
        if String.length x = 1 && Rfc5322.is_military_zone x.[0]
        then Military_zone x.[0]
        else Fmt.invalid_arg "invalid zone %s" x
end

type t = Rfc5322.date =
  { day  : Day.t option
  ; date : int * Month.t * int
  ; time : int * int * int option
  ; zone : Zone.t }

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
