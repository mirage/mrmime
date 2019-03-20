(** Location module. *)

type point = int
(** Type of point to a flow. *)

type zone = { a : int; b : int; }
(** Type of zone to a flow. *)

type t
(** Type of location to a flow *)

type 'a with_location
type 'a w = 'a with_location

val make : point -> point -> t
(** [make a b] makes a new location of [b - a] byte(s) starting at [a]. *)

val some : zone -> t
(** [some z] makes a new location from a {!zone} [z]. *)

val none : t
(** [none] is a unknowable location. *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

val left : t -> point option
(** [left location] returns starting index of [location]. *)

val left_exn : t -> point
(** [left_exn location] is same as {!left} but it raises an exception if
    [location] is an unknowable location (see {!none}). *)

val right : t -> point option
(** [right location] returns stopping index of [location]. *)

val right_exn : t -> point
(** [right_exn location] is same as {!right} but it raises an exception if
    [location] is an unknowable location (see {!none}). *)

val length : t -> int option
(** [length location] returns length of location - how many bytes we have on
    [location]. *)

val length_exn : t -> int
(** Same as {!length} but it raises an exception if [location] is an unknowable
    location (see {!none}). *)

val with_location : location:t -> 'a -> 'a with_location
val inj : location:t -> 'a -> 'a with_location

val without_location : 'a with_location -> 'a
val prj : 'a with_location -> 'a

val location : 'a with_location -> t

val union : t -> t -> t
