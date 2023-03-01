open Crowbar
open Common

(* XXX(dinosaure): we did not generate UTF-8 valid string - we refer only on RFC 822. *)

let token = alphabet_from_predicate Mrmime.Content_type.is_token
let qtext = alphabet_from_predicate Mrmime.Content_type.is_qtext
let token = dynamic_bind (range ~min:1 32) (string_from_alphabet token)

let value =
  map [ token ] (fun v ->
      match Mrmime.Content_type.Parameters.value v with
      | Ok v -> v
      | Error _ -> bad_test ())

(* XXX(dinosaure): IETF token does not exists - see [Rfc2045.ty] *)

let x_token =
  map
    [ choose [ const "x"; const "X" ]; token ]
    (fun head tail -> head ^ "-" ^ tail)

let ty =
  choose
    [ const `Text;
      const `Image;
      const `Audio;
      const `Video;
      const `Application;
      const `Message;
      const `Multipart;
      map [ x_token ] (fun v -> `X_token v)
    ]

let iana ty =
  choose
    (Mrmime.Iana.Map.find
       (Mrmime.Content_type.Type.to_string ty)
       Mrmime.Iana.database
    |> Mrmime.Iana.Set.elements
    |> List.map const)

let key =
  map [ token ] (fun v ->
      match Mrmime.Content_type.Parameters.key v with
      | Ok v -> v
      | Error _ -> bad_test ())

let subty = function
  | (#Mrmime.Content_type.Type.discrete | #Mrmime.Content_type.Type.composite)
    as ty ->
      map [ iana ty ] (fun v -> (ty, `Iana_token v))
  | ty -> map [ x_token ] (fun v -> (ty, `X_token v))

let parameter = map [ key; value ] (fun key value -> (key, value))
let parameters = list parameter

let content_type =
  map
    [ dynamic_bind ty subty; parameters ]
    (fun (ty_, subty_) parameters_ ->
      Mrmime.Content_type.{ ty = ty_; subty = subty_; parameters = parameters_ })

module BBuffer = Buffer

let emitter_of_buffer buf =
  let open Prettym in
  let write a = function
    | { IOVec.buffer = Buffer.String x; off; len } ->
        BBuffer.add_substring buf x off len;
        a + len
    | { IOVec.buffer = Buffer.Bytes x; off; len } ->
        BBuffer.add_subbytes buf x off len;
        a + len
    | { IOVec.buffer = Buffer.Bigstring x; off; len } ->
        BBuffer.add_string buf (Bigstringaf.substring x ~off ~len);
        a + len
  in
  List.fold_left write 0

let ( <.> ) f g x = f (g x)

let parser buf =
  let open Angstrom in
  Unstrctrd_parser.unstrctrd buf >>= fun v ->
  let res =
    let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
    Unstrctrd.without_comments v
    >>| Unstrctrd.fold_fws
    >>| Unstrctrd.to_utf_8_string
    >>= (Result.map_error (fun x -> `Msg x)
        <.> Angstrom.parse_string ~consume:Prefix
              Mrmime.Content_type.Decoder.content)
  in
  match res with Ok v -> return v | Error (`Msg err) -> fail err

let () =
  let open Mrmime in
  Crowbar.add_test ~name:"content-type" [ content_type ] @@ fun content_type ->
  let buffer = Buffer.create 0x100 in
  let encoder =
    Prettym.create ~margin:78 ~new_line:"\r\n" 0x100
      ~emitter:(emitter_of_buffer buffer)
  in
  let encoder =
    Prettym.keval Prettym.flush encoder
      Prettym.[ !!Content_type.Encoder.content_type; new_line ]
      content_type
  in

  check_eq ~pp:Fmt.bool ~eq:( = ) (Prettym.is_empty encoder) true;

  let result = Buffer.contents buffer in
  let buf = Bytes.create 0x7f in

  match Angstrom.parse_string ~consume:Prefix (parser buf) result with
  | Ok content_type' ->
      check_eq ~pp:Content_type.pp ~eq:Content_type.equal content_type
        content_type'
  | Error err ->
      failf "%a can not be parsed: %s" Content_type.pp content_type err
