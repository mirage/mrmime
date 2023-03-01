let ( <.> ) f g x = f (g x)

let parse_content_type x =
  let parser =
    let open Angstrom in
    let failf fmt = Fmt.kstr fail fmt in
    let buf = Bytes.create 0x7f in
    Unstrctrd_parser.unstrctrd buf >>= fun v ->
    let res =
      let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
      Unstrctrd.without_comments v
      >>| Unstrctrd.fold_fws
      >>| Unstrctrd.to_utf_8_string
      >>= (Result.map_error (fun x -> `Msg x)
          <.> Angstrom.parse_string ~consume:Angstrom.Consume.Prefix
                Mrmime.Content_type.Decoder.content)
    in
    match res with
    | Ok v -> return v
    | Error (`Msg err) -> failf "Invalid Content-Type (%s)" err
  in
  Angstrom.parse_string ~consume:Angstrom.Consume.Prefix parser (x ^ "\r\n")

let content_type =
  Alcotest.testable Mrmime.Content_type.pp Mrmime.Content_type.equal

let make raw expect =
  Alcotest.test_case raw `Quick @@ fun () ->
  match parse_content_type raw with
  | Ok value -> Alcotest.(check content_type) raw expect value
  | Error err -> Fmt.invalid_arg "Invalid content-type value: %s (%s)." raw err

let content_type_0 =
  let open Mrmime.Content_type in
  let value =
    let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value "us-ascii" >>= fun us_ascii ->
    Subtype.iana Type.text "plain" >>| fun subty ->
    make Type.text subty Parameters.(add charset us_ascii empty)
  in
  Result.get_ok value

let content_type_1 =
  let open Mrmime.Content_type in
  let value =
    let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value "us-ascii" >>= fun us_ascii ->
    Subtype.iana Type.text "plain" >>| fun subty ->
    make Type.text subty Parameters.(add charset us_ascii empty)
  in
  Result.get_ok value

let content_type_2 =
  let open Mrmime.Content_type in
  let value =
    let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value (Rosetta.encoding_to_string `ISO_8859_1) >>= fun latin1 ->
    Subtype.iana Type.text "plain" >>| fun subty ->
    make Type.text subty Parameters.(add charset latin1 empty)
  in
  Result.get_ok value

let () =
  Alcotest.run "rfc2045"
    [ ( "content-type",
        [ make "text/plain; charset=us-ascii (Plain text)" content_type_0;
          make "text/plain; charset=\"us-ascii\"" content_type_1;
          make "text/plain; charset=ISO-8859-1" content_type_2
        ] )
    ]
