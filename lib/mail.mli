type field_mail = [ Rfc5322.field | Rfc5322.resent | Rfc5322.trace | Rfc2045.field | Rfc2045.field_version | Rfc5322.unsafe | Rfc5322.lines ]
type field_part = [ Rfc5322.field | Rfc5322.resent | Rfc5322.trace | Rfc2045.field | Rfc5322.unsafe | Rfc5322.lines ]

type ('discrete, 'extension) t =
  | Discrete of { content : Content.t
                ; fields : Garbage.t
                ; body : 'discrete }
  | Extension of { content : Content.t
                 ; fields : Garbage.t
                 ; body : 'extension }
  | Multipart of { content : Content.t
                 ; fields : Garbage.t
                 ; parts : ('discrete, 'extension) atom list }
  | Message of { content : Content.t
               ; header : Header.t
               ; fields : Garbage.t
               ; message : ('discrete, 'extension) t }
and ('discrete, 'extension) part =
  | Part_discrete of 'discrete
  | Part_extension of 'extension
  | Part_multipart of ('discrete, 'extension) atom list
  | Part_message of { header : Header.t
                    ; message : ('discrete, 'extension) t }
and ('discrete, 'extension) atom =
  { content : Content.t
  ; fields : (Number.t * field_part * Location.t) list
  ; part : ('discrete, 'extension) part option }
and garbage =
  [ `Unsafe of Field_name.t * Unstructured.t
  | `Lines of (string * Location.t) list ]

type ('valid, 'invalid) contents =
  | Contents of 'valid
  | Invalid of 'invalid

type mail = ((string, string) contents, string) t
type 'id stream = ('id, 'id) t

val header : (Header.t * Garbage.t) Angstrom.t
(** Angstrom parser of a RFC 5322 header. *)

val heavy_octet : string option -> Content.t -> (string, string) contents Angstrom.t
(** {i Heavy} parser of a body - it will stores bodies into [string]. *)

val light_octet : emitter:(string option -> unit) -> string option -> Content.t -> unit Angstrom.t
(** {i Light} parser of body - it sends contents to given [emitter]. *)

val mail : (Header.t * mail) Angstrom.t
(** Angstrom parser of an entire RFC 5322 mail (including header). *)

type 'id emitters = Content.t -> (string option -> unit) * 'id

val stream : emitters:(Content.t -> (string option -> unit) * 'id) -> (Header.t * 'id stream) Angstrom.t
(** [stream ~emitters] is an Angstrom parser of an entire RFC 5322 mail which
   will use given emitters by [emitters] to store bodies. *)
