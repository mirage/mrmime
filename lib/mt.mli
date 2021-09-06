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

type 'x stream = unit -> 'x option
type buffer = string * int * int
type field = Field_name.t * Unstructured.t
type part
type multipart
type 'g rng = ?g:'g -> int -> string

val rng : int array rng
(** A default random generator. *)

val part : ?encoding:bool -> ?header:Header.t -> buffer stream -> part
(** [part ?encoding ?header stream] makes a new part from a body stream [stream],
   with the given [header] (default to {!Header.empty}). If [header] contains
   a [Content-Transfer-Encoding] and [encoding = true] (default), the stream will
   be {b encoded} to the given encoding. Otherwise, the stream is not transencoded.

   The underlying behavior of the encoding depends on what the user choose:
   - For a [base64] encoding, the stream is encoded as is and the resulted
     stream emits the body line-per-line;
   - For a [raw] encoding, the stream is added as is and should respect the
     line-per-line emission;
   - For a [quoted-printable], a chunk of the given stream is considered as
     a line regardless the newline convention of the operating system
     (and the encoder put a line-break according to the RFC 2045).

   Despite the [base64] encoding which can safely be used independantly of
   the behavior of the given stream, [7bit]/[8bit] and [quoted-printable] expect
   a specific stream line-per-line. [7bit]/[8bit] should include the [CRLF]
   newline per emission and the [quoted-printable] considers a emitted chunk
   as a line (without the newline character). *)

val multipart :
  ?g:'g ->
  rng:'g rng ->
  ?header:Header.t ->
  ?boundary:string ->
  part list ->
  multipart
(** [multipart ~rng ~content ~boundary ~fields parts] makes a new multipart from
   a bunch of parts, specified [Content-*] fields, others [fields] and a
   specified [boundary]. If [boundary] is not specifed, we use [rng] to make a
   random boundary (we did not check that it does not appear inside [parts]). *)

val multipart_as_part : multipart -> part
(** [multipart_as_part m] compiles a multipart [m] as a part. *)

type 'x body

val simple : part body
val multi : multipart body

type t
(** Type of mail. *)

val make : Header.t -> 'x body -> 'x -> t
(** [make header kind v] makes a mail from an [header] and a element which can
   be a {!simple} part or a {!multi}part. *)

val to_stream : t -> buffer stream
(** [to_stream mail] generates a stream from a mail. *)
