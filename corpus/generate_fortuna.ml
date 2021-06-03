module Generate = Fuzz.Make (Fortuna)
open Utils

let generate seed dst =
  let g = Mirage_crypto_rng.Fortuna.create () in
  Mirage_crypto_rng.Fortuna.reseed ~g (Cstruct.of_string seed);
  assert (Mirage_crypto_rng.Fortuna.seeded ~g);
  (* let mail = Fortuna.run ~g Generate.mail in
       let stream = Mrmime.Mt.to_stream mail in
       let str = stream_to_string stream in
     let ret =
         match Angstrom.parse_string ~consume:All Mrmime.Mail.mail str with
         | Ok _ -> `Ok 0
         | Error _ ->
             Fmt.epr "Invalid mail: @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.default) str;
             assert false
       in
  *)
  let str = Fortuna.run ~g Generate.mailbox |> Mrmime.Mailbox.to_string in
  let ret = `Ok 0 in
  let oc, oc_close = build_oc dst in
  output_string oc str;
  oc_close oc;
  ret
