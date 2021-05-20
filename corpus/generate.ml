module Generate = Fuzz.Make (Fortuna)

let parsers =
  let open Mrmime in
  let unstructured = Field.(Witness Unstructured) in
  let open Field_name in
  Map.empty
  |> Map.add date unstructured
  |> Map.add from unstructured
  |> Map.add sender unstructured
  |> Map.add reply_to unstructured
  |> Map.add (v "To") unstructured
  |> Map.add cc unstructured
  |> Map.add bcc unstructured
  |> Map.add subject unstructured
  |> Map.add message_id unstructured
  |> Map.add comments unstructured
  |> Map.add content_type unstructured
  |> Map.add content_encoding unstructured

let stream_to_string v =
  let buf = Buffer.create 0x1000 in
  let rec go () =
    match v () with
    | Some (str, off, len) ->
        Buffer.add_substring buf str off len;
        go ()
    | None -> Buffer.contents buf
  in
  go ()

let generate seed dst =
  let g = Mirage_crypto_rng.Fortuna.create () in
  Mirage_crypto_rng.Fortuna.reseed ~g (Cstruct.of_string seed);
  assert (Mirage_crypto_rng.Fortuna.seeded ~g);
  let mail = Fortuna.run ~g Generate.mail in
  let stream = Mrmime.Mt.to_stream mail in
  let str = stream_to_string stream in
  let ret =
    match Angstrom.parse_string ~consume:All Mrmime.Mail.mail str with
    | Ok _ -> `Ok 0
    | Error _ ->
        Fmt.epr "Invalid mail: @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.default) str;
        assert false
  in
  let oc, oc_close =
    match dst with
    | `Standard -> (stdout, ignore)
    | `Filename filename ->
        let oc = open_out (Fpath.to_string filename) in
        (oc, close_out)
  in
  output_string oc str;
  oc_close oc;
  ret

open Cmdliner

let base64 =
  Arg.conv
    ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)

let filename =
  let parser = function
    | "-" -> Ok `Standard
    | str -> Rresult.(Fpath.of_string str >>| fun v -> `Filename v)
  in
  let pp ppf = function
    | `Standard -> Fmt.string ppf "-"
    | `Filename v -> Fpath.pp ppf v
  in
  Arg.conv (parser, pp)

let seed =
  let doc = "Fortuna seed." in
  Arg.(required & opt (some base64) None & info [ "s"; "seed" ] ~doc)

let output = Arg.(value & pos ~rev:true 0 filename `Standard & info [])

let cmd =
  let doc = "Generate a valid email from a seed." in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "Generate a random email from the $(i,fortuna) random number generator \
         and the $(i,base64) given seed.";
    ]
  in
  (Term.(ret (const generate $ seed $ output)), Term.info "generate" ~doc ~man)

let () = Term.(exit_status @@ eval cmd)
