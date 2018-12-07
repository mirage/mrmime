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
end

module Zone = struct
  type t = Rfc5322.zone =
    | UT  | GMT
    | EST | EDT
    | CST | CDT
    | MST | MDT
    | PST | PDT
    | Military_zone of char
    | TZ of int

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

  let tz x = Some (TZ x) (* TODO *)

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
    | TZ z -> Fmt.pf ppf "(TZ %04d)" z
    | Military_zone c -> Fmt.pf ppf "(Military_zone %c)" c
end

type t = Rfc5322.date =
  { day  : Day.t option
  ; date : int * Month.t * int
  ; time : int * int * int option
  ; zone : Zone.t }

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


