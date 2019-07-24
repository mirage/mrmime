module Refl : sig type ('a, 'b) t = Refl : ('a, 'a) t end

module Value : sig
  type phrase_or_message_id =
    [ `Phrase of Rfc5322.phrase
    | `MessageID of Rfc822.nonsense Rfc822.msg_id]

  type trace =
    Rfc5322.mailbox option
    * ([ `Addr of Rfc5322.mailbox
       | `Domain of Rfc5322.domain
       | `Word of Rfc822.word ] list * Rfc5322.date option) list

  type 'a t =
    | Date : Rfc5322.date t
    | Mailboxes : Rfc5322.mailbox list t
    | Mailbox : Rfc5322.mailbox t
    | Addresses : Rfc5322.address list t
    | MessageID : Rfc822.nonsense Rfc822.msg_id t
    | Phrase_or_message_id : phrase_or_message_id list t
    | Unstructured : Rfc5322.unstructured t
    | Phrases : Rfc5322.phrase list t
    | Trace : trace t

  type binding =
    | Field : Field_name.t * 'a t * 'a * Location.t -> binding
    | Lines : (string * Location.t) list * Location.t -> binding

  type value = V : 'a t -> value
  (* XXX(dinosaure): find an other name than [value], TODO! *)

  val of_string : string -> (value, Rresult.R.msg) result
  val pp : 'a t Fmt.t
  val pp_of_value : 'a t -> 'a Fmt.t
  val equal : 'a t -> 'b t -> ('a, 'b) Refl.t option
end

type 'value decoder

type 'value decode =
  [ `Field of Field_name.t * 'value
  | `Other of Field_name.t * string
  | `Lines of (string * Location.t) list
  | `Await
  | `End of string
  | `Malformed of string ]

val decoder : field_name:Field_name.t -> 'value Value.t -> Bigstringaf.t -> 'value decoder
(** [decoder ~field_name witness queue] make a new header decoder which will
   record any [field_name] which contains a ['v] correspoding to the type
   witness [witness]. Length of [queue] must be a power of two. *)

val decode : 'value decoder -> 'value decode
(** [decode decoder] decodes given inputs and returns:

    {ul
    {- [`Await] if we expect more inputs (then, a call to {!src} is needed).}
    {- [`End] when we reach end of the header (signaled by two [CRLF] tokens)
   with epilogue ([string] which is not a part of the header - and should be a
   part of the body).}
    {- [`Lines (line, location)] when we can not parse inputs as a binding of a
   field name and a value separated by a colon.}
    {- [`Field (field_name, value)] when we reach expected [field_name] (see
   {!decoder}) with a well-formed already {i parsed} value (see type witness of
   {!decoder}).}
    {- [`Other (field_name, value)] when we reach a binding of a field-name with
   a {i non-parsed} value. [field_name] can be equal to expected field-name
   notified to {!decoder} but we are not able to parse value correctly according
   to type witness expected.}} *)

val src : 'value decoder -> string -> int -> int -> (unit, Rresult.R.msg) result
(** [src decoder v off len] refills [decoder] with [v] starting at [off] with
   [len] bytes. inputs must be smaller than the given space in {!decoder}. *)
