(** Line is a raw line (which terminates with a CRLF) in the header-part of a
    mail which we did not find a field. *)

type t = string

val pp : t Fmt.t
