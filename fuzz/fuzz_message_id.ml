open Crowbar
open Common

(* XXX(dinosaure): we did not generate UTF-8 valid string - we refer only on RFC 822. *)

let local_word =
  map
    [ dynamic_bind (range ~min:1 78) (string_from_alphabet atext) ]
    (fun str ->
      match Mrmime.Mailbox.Local.word str with
      | Ok str -> str
      | Error _ -> bad_test ())

let local = list1 local_word

let domain_atom =
  map
    [ dynamic_bind (range ~min:1 78) (string_from_alphabet dtext) ]
    (fun input ->
      match Mrmime.Mailbox.Domain.atom input with
      | Ok v -> v
      | Error _ -> bad_test ())

let domain =
  map
    [ list1 domain_atom ]
    (fun lst -> `Domain (List.map (fun (`Atom x) -> x) lst))

(* XXX(dinosaure): we did not include [`Literal] domain because [Rfc822.domain]
   excludes it according to RFC 5321 (see [Rfc822.domain]). *)

let message_id = map [ local; domain ] (fun local domain -> (local, domain))

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
              Mrmime.MessageID.Decoder.message_id)
  in
  match res with Ok v -> return v | Error (`Msg err) -> fail err

let () =
  let open Mrmime in
  Crowbar.add_test ~name:"message_id" [ message_id ] @@ fun message_id ->
  let buffer = Buffer.create 0x100 in
  let encoder =
    Prettym.create ~margin:78 ~new_line:"\r\n" 0x100
      ~emitter:(emitter_of_buffer buffer)
  in
  let encoder =
    Prettym.keval Prettym.flush encoder
      Prettym.[ !!MessageID.Encoder.message_id; new_line ]
      message_id
  in

  check_eq ~pp:Fmt.bool ~eq:( = ) (Prettym.is_empty encoder) true;

  let result = Buffer.contents buffer in
  let buf = Bytes.create 0x7f in

  match Angstrom.parse_string ~consume:Prefix (parser buf) result with
  | Ok message_id' ->
      check_eq ~pp:MessageID.pp ~eq:MessageID.equal message_id message_id'
  | Error err ->
      Fmt.epr "message-id: @[<hov>%a@]\n%!" MessageID.pp message_id;
      Fmt.epr "output: @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.default) result;
      failf "%a can not be parsed: %s" MessageID.pp message_id err
