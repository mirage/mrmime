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

let phrase_word =
  map
    [ dynamic_bind (range ~min:1 78) bytes_fixed ]
    (fun str ->
      match Mrmime.Mailbox.Phrase.word str with
      | Ok elt -> elt
      | Error _ -> bad_test ())

let encoded_word =
  map [ bool; bytes ] (fun base64 input ->
      match base64 with
      | true -> Mrmime.Mailbox.Phrase.e ~encoding:Mrmime.Encoded_word.b input
      | false -> Mrmime.Mailbox.Phrase.e ~encoding:Mrmime.Encoded_word.q input)

let phrase =
  map
    [ choose [ phrase_word ]; list (choose [ const `Dot; phrase_word ]) ]
    (fun head -> function [] -> [ head ] | rest -> head :: rest)

let extension =
  map
    [ dynamic_bind (range 78) (string_from_alphabet ldh_str);
      range (String.length let_dig);
      dynamic_bind (range ~min:1 78) (string_from_alphabet dcontent)
    ]
    (fun ldh idx x ->
      match
        Mrmime.Mailbox.Domain.(
          make extension (ldh ^ String.make 1 let_dig.[idx], x))
      with
      | Ok v -> v
      | Error _ -> bad_test ())

let ipv4 =
  map [ bytes ] (fun input ->
      match Ipaddr.V4.of_string input with
      | Ok x -> Mrmime.Mailbox.Domain.(v ipv4 x)
      | Error _ -> bad_test ())

let ipv6 =
  map [ bytes ] (fun input ->
      match Ipaddr.V6.of_string input with
      | Ok x -> Mrmime.Mailbox.Domain.(v ipv6 x)
      | Error _ -> bad_test ())

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

let domain = choose [ extension; ipv4; ipv6; domain ]

(* XXX(dinosaure): we did not include [`Literal] domain because [Rfc822.domain]
   excludes it according to RFC 5321 (see [Rfc822.domain]). *)

let mailbox =
  map
    [ option phrase; local; list1 domain ]
    (fun name local domains ->
      match domains with
      | x :: r -> Emile.{ name; local; domain = (x, r) }
      | [] -> bad_test ())

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

let parser buf =
  let open Angstrom in
  Unstrctrd_parser.unstrctrd buf >>= fun v ->
  let res =
    let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
    Result.ok v
    >>| Unstrctrd.fold_fws
    (* XXX(dinosaure): '(' and ')' can be handle (and have a signification) by
       [Emile]. *)
    >>| Unstrctrd.to_utf_8_string
    >>= (Result.map_error (fun x -> `Msg x)
        <.> Angstrom.parse_string ~consume:Prefix Mrmime.Mailbox.Decoder.mailbox
        )
  in
  match res with Ok v -> return v | Error (`Msg err) -> fail err

let () =
  let open Mrmime in
  Crowbar.add_test ~name:"mailbox" [ mailbox ] @@ fun mailbox ->
  let buffer = Buffer.create 0x100 in
  let encoder =
    Prettym.create ~margin:78 ~new_line:"\r\n" 0x100
      ~emitter:(emitter_of_buffer buffer)
  in
  let encoder =
    Prettym.keval Prettym.flush encoder
      Prettym.[ !!Mailbox.Encoder.mailbox; new_line ]
      mailbox
  in

  check_eq ~pp:Fmt.bool ~eq:( = ) (Prettym.is_empty encoder) true;

  let result = Buffer.contents buffer in
  let buf = Bytes.create 0x7f in

  match Angstrom.parse_string ~consume:Prefix (parser buf) result with
  | Ok mailbox' -> check_eq ~pp:Mailbox.pp ~eq:Mailbox.equal mailbox mailbox'
  | Error err ->
      Fmt.epr "output: @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.default) result;
      failf "%a can not be parsed: %s" Mailbox.pp mailbox err
