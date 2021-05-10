module Generate = Fuzz.Make (Fortuna)

let rec decode_string str decoder =
  let open Mrmime in
  match Hd.decode decoder with
  | `End _ -> `Ok 0
  | `Field _v -> decode_string str decoder
  | `Malformed _err -> `Error (false, "Invalid generated email.")
  | `Await ->
      Hd.src decoder str 0 (String.length str);
      decode_string str decoder

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

let generate seed dst =
  let g = Mirage_crypto_rng.Fortuna.create () in
  Mirage_crypto_rng.Fortuna.reseed ~g (Cstruct.of_string seed);
  assert (Mirage_crypto_rng.Fortuna.seeded ~g);
  let hdr = Generate.header g in
  let str = Prettym.to_string Mrmime.Header.Encoder.header hdr in
  let decoder = Mrmime.Hd.decoder parsers in
  let ret = decode_string (str ^ "\r\n") decoder in
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
