(** MIME-Version module. *)

type t = Rfc2045.version
(** Type of MIME-Version. *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

val default : t
(** Default value of MIME-Version according to RFC 2045. *)
