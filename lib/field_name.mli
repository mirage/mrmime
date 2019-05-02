type t = private string
(** Type of field name. *)

val compare : t -> t -> int
(** Comparison function on {!t}. *)

val equal : t -> t -> bool
(** Equal function on {!t}. *)

val capitalize : t -> t
(** [capitalize t] capitalizes [t] - any letter in front of ['-'] will be a
   capital letter. *)

val canonicalize : t -> t
(** [canonicalize t] apply [String.lowercase_ascii] on [t]. *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] tries to return a field-name. [s] must
   respect standards. Otherwise, we return an [Error]. *)

val of_string_exn : string -> t
(** Same as {!of_string} but raises an {!Invalid_argument} instead of returning
   [Error]. *)

val v : string -> t
(** Alias of {!of_string_exn}. *)

val prefixed_by : string -> t -> bool
(** [of_prefixed_by prefix t] returns [true] if [t] is prefixed by [s]:

    {[
      prefixed_by "Resent" resent_date ;;
      - : bool = true
    ]} *)

(** {2 Encoder of field name.} *)

module Encoder : sig
  val field : t Encoder.encoding
end

(** {2 Helpers.} *)

(** {3 RFC 5322 Field names.} *)

val date : t
val from : t
val sender : t
val reply_to : t
val cc : t
val bcc : t
val subject : t
val message_id : t
val in_reply_to : t
val references : t
val comments : t
val keywords : t
val received : t
val return_path : t

(** {3 RFC 2045 Field names.} *)

val content_type : t
val content_encoding : t
val mime_version : t
val content_id : t
val content_description : t

(** {3 {i Resent} Field names.} *)

val resent_date : t
val resent_from : t
val resent_sender : t
val resent_to : t
val resent_cc : t
val resent_bcc : t
val resent_message_id : t
val resent_reply_to : t
