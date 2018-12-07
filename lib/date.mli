module Day : sig
  type t

  val mon : t
  val tue : t
  val wed : t
  val thu : t
  val fri : t
  val sat : t
  val sun : t

  val pp : t Fmt.t
end

module Month : sig
  type t

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
end

module Zone : sig
  type t

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
  val tz : int -> t option

  val pp : t Fmt.t
end

type t = Rfc5322.date

val pp : t Fmt.t
