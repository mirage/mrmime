let stream_of_string s =
  let once = ref false in
  (fun () -> if !once then None else ( once := true ; Some (s, 0, String.length s)))

let stream_of_random ?(chunk= 128) len =
  let ic = open_in "/dev/urandom" in
  let ln = ref 0 in
  let closed = ref false in
  let rs = Bytes.create chunk in
  let go () =
    let len = min (len - !ln) chunk in
    if len == 0 then ( if not !closed then close_in ic ; closed := true ; None )
    else ( really_input ic rs 0 len
         ; ln := !ln + len
         ; Some (Bytes.unsafe_to_string rs, 0, len) ) in
  go

let stream_to_string s =
  let b = Buffer.create 4096 in
  let rec go () = match s () with
    | Some (buf, off, len) ->
      Buffer.add_substring b buf off len ; go ()
    | None -> Buffer.contents b in
  go ()

let example0 =
  let open Mrmime in
  let john = let open Mailbox in Local.[ w "john" ] @ Domain.(domain, [ a "gmail"; a "com" ]) in
  let thomas = let open Mailbox in Local.[ w "thomas" ] @ Domain.(domain, [ a "gazagnaire"; a "com" ]) in
  let anil = let open Mailbox in Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ]) in

  let content0 =
    let open Content in
    let c =
      let open Content_type in
      make `Application (Subtype.v `Application "pdf") Parameters.(of_list [ k "filename", v "prg.exe" ]) in
    make ~encoding:`Base64 c in

  let content1 =
    let open Content in
    let c =
      let open Content_type in
      make `Text (Subtype.v `Text "plain") Parameters.(of_list [ k "charset", v "utf-8" ]) in
    make ~encoding:`Quoted_printable c in

  let subject =
    let open Unstructured in
    [ v "First"; sp 1; v "email" ] in

  let now = Date.of_ptime ~zone:Date.Zone.GMT (Ptime_clock.now ()) in
  let part0 = Mt.part ~content:content0 (stream_of_random 4096) in
  let part1 = Mt.part ~content:content1 (stream_of_string "Hello World!") in
  let multipart = Mt.multipart ~rng:Mt.rng [ part0; part1 ] in

  let header =
    let open Header in
    Field.(Sender $ john)
    & Field.(To $ Address.[ mailbox thomas; mailbox anil ])
    & Field.(Subject $ subject)
    & Field.(Date $ now)
    & empty in

  Mt.make header Mt.multi multipart

let example1 =
  let open Mrmime in
  let john = let open Mailbox in Local.[ w "john" ] @ Domain.(domain, [ a "gmail"; a "com" ]) in
  let thomas = let open Mailbox in Local.[ w "thomas" ] @ Domain.(domain, [ a "gazagnaire"; a "org" ]) in
  let anil = let open Mailbox in Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ]) in
  let hannes = let open Mailbox in Local.[ w "hannes" ] @ Domain.(domain, [ a "mehnert"; a "org" ]) in
  let gemma = let open Mailbox in Local.[ w "gemma"; w "t"; w "gordon" ] @ Domain.(domain, [ a "gmail"; a "com" ]) in

  let content0 =
    let open Content in
    let c =
      let open Content_type in
      make `Text (Subtype.v `Text "plain") Parameters.(of_list [ k "charset", v "utf-8" ]) in
    make ~encoding:`Quoted_printable c in

  let subject =
    let open Unstructured in
    [ v "Second"; sp 1; v "email" ] in

  let now = Date.of_ptime ~zone:Date.Zone.GMT (Ptime_clock.now ()) in
  let part = Mt.part ~content:content0 (stream_of_string "Hello World!") in

  let header =
    let open Header in
    Field.(Sender $ john)
    & Field.(To $ Address.[ mailbox thomas; mailbox anil; mailbox hannes; mailbox gemma ])
    & Field.(Subject $ subject)
    & Field.(Date $ now)
    & empty in

  Mt.make header Mt.simple part

let test0 () =
  Alcotest.test_case "example 0" `Quick @@ fun () ->
  let res0 = stream_to_string (Mrmime.Mt.to_stream example0) in
  match Angstrom.parse_string Mrmime.Mail.mail res0 with
  | Ok _ -> Fmt.epr "%s%!" res0
  | Error _ -> Fmt.invalid_arg "Generate unparsable email"

let test1 () =
  Alcotest.test_case "example 1" `Quick @@ fun () ->
  let res0 = stream_to_string (Mrmime.Mt.to_stream example1) in
  match Angstrom.parse_string Mrmime.Mail.mail res0 with
  | Ok mail ->
    Fmt.epr "%s%!" res0 ;
    let gemma_exists (header, _) =
      let open Mrmime in
      let gemma = let open Mailbox in Local.[ w "gemma"; w "t"; w "gordon" ] @ Domain.(domain, [ a "gmail"; a "com" ]) in
      match Header.get (Field_name.v "To") header with
      | (_, Header.Value (Field.Addresses, v), _) :: _ ->
        if List.exists Address.(equal (mailbox gemma)) v
        then () else Fmt.invalid_arg "Gemma does not exist"
      | _ -> Fmt.invalid_arg "Field \"To\" does not exist" in
    gemma_exists mail
  | Error _ -> Fmt.invalid_arg "Generate unparsable email"

let () =
  Alcotest.run "mail"
    [ "example", [ test0 (); test1 (); ] ]
