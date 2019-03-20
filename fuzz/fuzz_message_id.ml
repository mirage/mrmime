open Crowbar

let (<.>) f g = fun x -> f (g x)

let char_from_alphabet alphabet =
  map [ range (String.length alphabet) ] (String.make 1 <.> String.get alphabet)

let string_from_alphabet alphabet len =
  let rec go acc = function
    | 0 -> concat_gen_list (const "") acc
    | n -> go (char_from_alphabet alphabet :: acc) (pred n) in
  go [] len

let alphabet_from_predicate predicate =
  let len =
    let rec go acc = function
      | 0 -> if predicate (Char.unsafe_chr 0) then acc + 1 else acc
      | n ->
        let acc = if predicate (Char.unsafe_chr n) then acc + 1 else acc in
        go acc (n - 1) in
    go 0 255 in
  let res = Bytes.create len in
  let rec go idx = function
    | 0 ->
      if predicate (Char.unsafe_chr 0) then Bytes.unsafe_set res idx (Char.unsafe_chr 0)
    | n ->
      if predicate (Char.unsafe_chr n) then Bytes.unsafe_set res idx (Char.unsafe_chr n) ;
      let idx = if predicate (Char.unsafe_chr n) then idx + 1 else idx in
      go idx (n - 1) in
  go 0 255 ; Bytes.unsafe_to_string res

(* XXX(dinosaure): we did not generate UTF-8 valid string - we refer only on RFC
   822. *)

let atext = alphabet_from_predicate Mrmime.Rfc822.is_atext
let dtext = alphabet_from_predicate Mrmime.Rfc822.is_dtext

let local_word =
  map [ dynamic_bind (range ~min:1 78) (string_from_alphabet atext) ]
    (fun str -> match Mrmime.Mailbox.Local.word str with
       | Some str -> str
       | None -> bad_test ())

let local = list1 local_word

let domain_atom = map [ dynamic_bind (range ~min:1 78) (string_from_alphabet dtext) ]
    (fun input -> match Mrmime.Mailbox.Domain.atom input with
       | Some v -> v
       | None -> bad_test ())

let domain = map [ list1 domain_atom ] (fun lst -> `Domain (List.map (fun (`Atom x) -> x) lst))

(* XXX(dinosaure): we did not include [`Literal] domain because [Rfc822.domain]
   excludes it according to RFC 5321 (see [Rfc822.domain]). *)

let message_id =
  map [ local; domain ]
    (fun local domain -> (local, domain))

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

  Crowbar.add_test ~name:"message_id" [ message_id ] @@ fun message_id ->

  let buffer = Buffer.create 0x100 in
  let encoder = Encoder.Level1.create ~margin:78 ~new_line:"\r\n" 0x100 in
  let encoder = Encoder.with_writer encoder (writer_of_buffer buffer) in
  let _ = Encoder.eval encoder Encoder.[ !!MessageID.Encoder.message_id; new_line; new_line ] message_id in
  let result = Buffer.contents buffer in

  match Angstrom.parse_string Angstrom.(Rfc822.msg_id ~address_literal:(fail "Invalid domain") <* Rfc822.crlf <* Rfc822.crlf) result with
  | Ok message_id' ->
    check_eq ~pp:MessageID.pp ~eq:MessageID.equal message_id message_id'
  | Error err ->
    failf "%a can not be parsed: %s" MessageID.pp message_id err
