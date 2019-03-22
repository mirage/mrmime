open Crowbar
open Common

(* XXX(dinosaure): we did not generate UTF-8 valid string - we refer only on RFC 822. *)

let token = alphabet_from_predicate Mrmime.Rfc2045.is_token
let qtext = alphabet_from_predicate Mrmime.Rfc822.is_qtext

let token = dynamic_bind (range ~min:1 32) (string_from_alphabet token)

let value =
  map [ token ]
    (fun v -> match Mrmime.Content_type.Parameters.value v with
       | Some v -> v
       | None -> bad_test ())

(* XXX(dinosaure): IETF token does not exists - see [Rfc2045.ty] *)

let x_token =
  map [ choose [ const "x"; const "X" ]
      ; token ]
    (fun head tail -> head ^ "-" ^ tail)

let ty =
  choose
    [ const `Text
    ; const `Image
    ; const `Audio
    ; const `Video
    ; const `Application
    ; const `Message
    ; const `Multipart
    ; map [ x_token ] (fun v -> `X_token v) ]

let iana ty =
  choose (Mrmime.Iana.Map.find (Mrmime.Rfc2045.ty_to_string ty) Mrmime.Iana.database
          |> Mrmime.Iana.Set.elements
          |> List.map const)

let key = map [ token ] (fun v -> match Mrmime.Content_type.Parameters.key v with
    | Some v -> v
    | None -> bad_test ())

let subty = function
  | (#Mrmime.Rfc2045.discrete
    | #Mrmime.Rfc2045.composite) as ty -> map [ iana ty ] (fun v -> ty, `Iana_token v)
  | ty -> map [ x_token ] (fun v -> ty, `X_token v)

let parameter = map [ key; value] (fun key value -> (key, value))

let parameters = list parameter

let content_type =
  map [ dynamic_bind ty subty; parameters; ]
    (fun (ty_, subty_) parameters -> Mrmime.Rfc2045.{ ty= ty_; subty= subty_; parameters })

let writer_of_buffer buf =
  let open Mrmime.Encoder in

  let write a = function
    | { Level0.IOVec.buffer= Level0.Buffer.String x; off; len; } ->
      Buffer.add_substring buf x off len; a + len
    | { Level0.IOVec.buffer= Level0.Buffer.Bytes x; off; len; } ->
      Buffer.add_subbytes buf x off len; a + len
    | { Level0.IOVec.buffer= Level0.Buffer.Bigstring x; off; len; } ->
      Buffer.add_string buf (Bigstringaf.substring x ~off ~len); a + len in
  List.fold_left write 0

let () =
  let open Mrmime in

  Crowbar.add_test ~name:"content-type" [ content_type ] @@ fun content_type ->

  let buffer = Buffer.create 0x100 in
  let encoder = Encoder.Level1.create ~margin:78 ~new_line:"\r\n" 0x100 in
  let encoder = Encoder.with_writer encoder (writer_of_buffer buffer) in
  let _ = Encoder.eval encoder Encoder.[ !!Content_type.Encoder.content_type; new_line; new_line; ] content_type in
  let result = Buffer.contents buffer in

  match Angstrom.parse_string Angstrom.(Rfc2045.content <* Rfc822.crlf <* Rfc822.crlf) result with
  | Ok content_type' ->
    check_eq ~pp:Content_type.pp ~eq:Content_type.equal content_type content_type'
  | Error err ->
    failf "%a can not be parsed: %s" Content_type.pp content_type err
