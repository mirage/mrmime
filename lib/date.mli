(*
 * Copyright (c) 2018-2019 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Day : sig
  type t =
    | Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun  (** Type of day according RFC 822 / RFC 2822 / RFC 5322. *)

  (** {3 Constructors.} *)

  val mon : t
  val tue : t
  val wed : t
  val thu : t
  val fri : t
  val sat : t
  val sun : t

  val to_string : t -> string
  (** [to_string v] returns a well-formed string from a day [v]. *)

  val of_string : string -> (t, [ `Msg of string ]) result
  (** [of_string v] returns a day from a well-formed string [v]. Process is
     case-sensitive. Day needs to be capitalized (eg. ["Fri"]). *)

  val of_string_exn : string -> t
  (** [of_string_exn v] returns a day from a well-formed string [v]. Process is
     case-sensitive. Day needs to be capitalized (eg. ["Fri"]).

      @raise [Invalid_argument] when [v] is an invalid day. *)

  val v : string -> t
  (** Alias of {!of_string_exn}. *)

  (** {3 Pretty-printers.} *)

  val pp : Format.formatter -> t -> unit

  (** {3 Equals.} *)

  val equal : t -> t -> bool
end

module Month : sig
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
    | Dec  (** Type of month according RFC 822 / RFC 2822 / RFC 5322. *)

  (** {3 Constructors.} *)

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
  (** [to_int m] is the number of the month [m]. *)

  val of_int : int -> (t, [ `Msg of string ]) result
  (** [of_int n] returns month of the number [n]. If [n < 1] or [n > 12], it
     returns an error. *)

  val of_int_exn : int -> t
  (** [of_int_exn] is an alias on {!of_int} except that if user give an invalid
     number, we raise an exception. *)

  val to_string : t -> string
  (** [to_stirng v] returns a well-formed string from a month [v]. *)

  val of_string : string -> (t, [ `Msg of string ]) result
  (** [of_string v] returns a month from a well-formed string [v]. Process is
     case-sensitive, month needs to be capitalized (eg. ["Nov"]). *)

  val of_string_exn : string -> t
  (** [of_string v] returns a month from a well-formed string [v]. Process is
     case-sensitive, month needs to be capitalized (eg. ["Nov"]).

     @raise [Invalid_argument] when [v] is an invalid month. *)

  val v : string -> t
  (** Alias of {!of_string_exn}. *)

  (** {3 Pretty-printers.} *)

  val pp : Format.formatter -> t -> unit

  (** {3 Equals.} *)

  val equal : t -> t -> bool
end

module Zone : sig
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
        (** Type of zone according RFC 822 / RFC 2822 / RFC 5322. *)

  (** {3 Constructors.} *)

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
  val military_zone : char -> (t, [ `Msg of string ]) result
  val tz : int -> int -> (t, [ `Msg of string ]) result

  val to_string : t -> string
  (** [to_string v] returns a well-formed string from a time zone [v]. *)

  val of_string : string -> (t, [ `Msg of string ]) result
  (** [of_string v] returns a time zone from a well-formed string [v]. Process is
     case-sensitive. *)

  val of_string_exn : string -> t
  (** [of_string_exn v] returns a time zone from a well-formed string [v].
     Process is case-sensitive.

      @raise [Invalid_argument] when [v] is an invalid time zone. *)

  val v : string -> t
  (** Alias of {!of_string_exn}. *)

  (** {3 Pretty-printers.} *)

  val pp : Format.formatter -> t -> unit

  (** {3 Equals.} *)

  val equal : t -> t -> bool
end

type t =
  { day : Day.t option;
    date : int * Month.t * int;
    time : int * int * int option;
    zone : Zone.t
  }
(** Type of date according RFC 822 / RFC 2822 / RFC 5322. *)

(** {2 Constructors.} *)

val make :
  ?day:Day.t ->
  int * Month.t * int ->
  int * int * int option ->
  Zone.t ->
  (t, [> `Msg of string ]) result
(** [make ?day (year, month, day) (hh, mm, ss) tz] returns a date corresponding
   to [month/day/year hh:mm:ss] date-time with time zone [tz]. [?day] (which is
   the day in the 7-day week) and [day] must correspond according of timestamp
   to [month/day/year] and time zone [tz]. If it's not the case, [make] returns
   an error.

   [(year, month, day) (hh, mm, ss)] must correspond to a valid POSIX timestamp.
   The date-time must be in the range of [0000-01-01 00:00:00 UTC] and
   [9999-12-31 23:59:59.99 UTC]. Otherwise, [make] returns an error.

   If [ss = None], seconds are [0]. If [?day = None], it will be the day in the
   7-day week of POSIX timestamp corresponding to date-time [(year, month, day)
   (hh, mm, ss)] expressed in the time zone offset [tz].

   [make] relies on {!Ptime.of_date_time}. To completely understand implication
   of that, you should read basics about [ptime]. *)

val to_ptime : t -> (Ptime.t * int, [> `Msg of string ]) result
(** [to_ptime t] returns a POSIX timestamp {!Ptime.t}. *)

val of_ptime : zone:Zone.t -> Ptime.t -> t
(** [of_ptime ~zone t] is date-time {!t} of POSIX timestamp [t]. [zone] hints
   the time zone offset used for the resulting daytime {!Day.t} component. *)

(** {2 Pretty-printers.} *)

val pp : Format.formatter -> t -> unit

(** {2 Equals & compares.} *)

val equal : t -> t -> bool
val compare : t -> t -> int

(** {2 Decoder of date.} *)

module Decoder : sig
  val date_time : t Angstrom.t
end

(** {2 Encoder of date.} *)

module Encoder : sig
  val date : t Prettym.t
end
