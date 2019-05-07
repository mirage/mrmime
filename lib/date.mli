module Day : sig
  type t = Rfc5322.day =
    | Mon | Tue | Wed
    | Thu | Fri | Sat
    | Sun

  val mon : t
  val tue : t
  val wed : t
  val thu : t
  val fri : t
  val sat : t
  val sun : t
  val pp : t Fmt.t
  val to_string : t -> string
  val of_string : string -> t

  val equal : t -> t -> bool
end

module Month : sig
  type t = Rfc5322.month =
    | Jan | Feb | Mar | Apr | May | Jun
    | Jul | Aug | Sep | Oct | Nov | Dec

  val jan : t
  val feb : t
  val mar : t
  val apr : t
  val may : t
  val jun : t
  val jul : t
  val aug : t
  val sep : t
  val oct : t
  val nov : t
  val dec : t
  val to_int : t -> int
  val of_int : int -> t option
  val pp : t Fmt.t
  val to_string : t -> string
  val of_string : string -> t

  val equal : t -> t -> bool
end

module Zone : sig
  type t = Rfc5322.zone =
    | UT  | GMT
    | EST | EDT
    | CST | CDT
    | MST | MDT
    | PST | PDT
    | Military_zone of char
    | TZ of int * int

  val ut : t
  val gmt : t
  val est : t
  val edt : t
  val cst : t
  val cdt : t
  val mst : t
  val mdt : t
  val pst : t
  val pdt : t
  val military_zone : char -> t option
  val tz : int -> int -> t option
  val pp : t Fmt.t
  val to_string : t -> string
  val of_string : string -> t

  val equal : t -> t -> bool
end

type t = Rfc5322.date =
  { day  : Day.t option
  ; date : int * Month.t * int
  ; time : int * int * int option
  ; zone : Zone.t }

val pp : t Fmt.t
val equal : t -> t -> bool
val compare : t -> t -> int
val make : ?day:Day.t -> (int * Month.t * int) -> (int * int * int option) -> Zone.t -> t option

module Encoder : sig
  val date : t Encoder.t
end

val to_unstructured : t -> Unstructured.t
