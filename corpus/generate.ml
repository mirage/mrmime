module Generate = Fuzz.Make (Fortuna)

let pp_witness : type a. a Mrmime.Field.t Fmt.t = fun ppf -> function
  | Mrmime.Field.Date -> Fmt.string ppf "date"
  | Mrmime.Field.Mailboxes -> Fmt.string ppf "mailboxes"
  | Mrmime.Field.Mailbox -> Fmt.string ppf "mailbox"
  | Mrmime.Field.Addresses -> Fmt.string ppf "addresses"
  | Mrmime.Field.MessageID -> Fmt.string ppf "messageID"
  | Mrmime.Field.Unstructured -> Fmt.string ppf "unstructured"
  | Mrmime.Field.Phrases -> Fmt.string ppf "phrases"
  | Mrmime.Field.Content -> Fmt.string ppf "content-type"
  | Mrmime.Field.Encoding -> Fmt.string ppf "content-transfer-encoding"

let show_field = let open Mrmime in function
  | Field.Field (field_name, w, _) ->
    Fmt.pr "[+] %a well-formed as %a.\n%!" Field_name.pp field_name pp_witness w

let rec decode_string str decoder =
  let open Mrmime in
  match Hd.decode decoder with
  | `End _ -> `Ok 0
  | `Field v ->
    show_field (Location.prj v) ; decode_string str decoder
  | `Malformed _err -> `Error (false, "Invalid generated email.")
  | `Await ->
    match Hd.src decoder str 0 (String.length str) with
    | Ok () -> decode_string str decoder
    | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

let generate seed =
  let g = Mirage_crypto_rng.Fortuna.create () in
  Mirage_crypto_rng.Fortuna.reseed ~g (Cstruct.of_string seed) ;
  assert (Mirage_crypto_rng.Fortuna.seeded ~g) ;
  let hdr = Generate.header g in
  let str = Prettym.to_string Mrmime.Header.Encoder.header hdr in
  let decoder = Mrmime.Hd.decoder (Bigstringaf.create 0x1000) in
  decode_string (str ^ "\r\n") decoder

open Cmdliner

let base64 =
  Arg.conv ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)

let seed = 
  let doc = "Fortuna seed." in
  Arg.(required & opt (some base64) None & info [ "s"; "seed" ] ~doc)

let cmd =
  let doc = "Generate a valid email from a seed." in
  let man =
    [ `S "DESCRIPTION"
    ; `P "Generate a random email from the $(i,fortuna) random number \
          generator and the $(i,base64) given seed." ] in
  Term.(ret (const generate $ seed)),
  Term.info "generate" ~doc ~man

let () = Term.(exit_status @@ eval cmd)
