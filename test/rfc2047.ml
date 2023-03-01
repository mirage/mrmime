let parse_encoded_word x =
  Angstrom.parse_string ~consume:Angstrom.Consume.All
    Mrmime.Encoded_word.Decoder.encoded_word x

let encoded_word =
  Alcotest.testable Mrmime.Encoded_word.pp Mrmime.Encoded_word.equal

let charset =
  Alcotest.testable Mrmime.Encoded_word.pp_charset
    Mrmime.Encoded_word.equal_charset

let encoding =
  Alcotest.testable Mrmime.Encoded_word.pp_encoding
    Mrmime.Encoded_word.equal_encoding

let data =
  Alcotest.testable
    (Fmt.Dump.result ~ok:Fmt.string ~error:(fun ppf (`Msg msg) ->
         Format.pp_print_string ppf msg))
    (Result.equal ~ok:String.equal ~error:(fun (`Msg _) (`Msg _) -> true))

let make raw (expect_charset, expect_encoding, expect_data) =
  Alcotest.test_case raw `Quick @@ fun () ->
  match parse_encoded_word raw with
  | Ok value ->
      Alcotest.(check charset)
        "charset"
        (Mrmime.Encoded_word.charset value)
        expect_charset;
      Alcotest.(check encoding)
        "encoding"
        (Mrmime.Encoded_word.encoding value)
        expect_encoding;
      Alcotest.(check data) "data" (Mrmime.Encoded_word.data value) expect_data
  | Error _ -> Fmt.invalid_arg "Invalid encoded-word: %s." raw

let () =
  Alcotest.run "rfc2047"
    [ ( "encoded-word",
        [ make "=?US-ASCII?Q?Keith_Moore?="
            (`US_ASCII, Mrmime.Encoded_word.q, Ok "Keith Moore");
          make "=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?="
            (`ISO_8859_1, Mrmime.Encoded_word.q, Ok "Keld Jørn Simonsen");
          make "=?ISO-8859-1?Q?Andr=E9_?="
            (`ISO_8859_1, Mrmime.Encoded_word.q, Ok "André ");
          make "=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?="
            (`ISO_8859_1, Mrmime.Encoded_word.b, Ok "If you can read this yo");
          make "=?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?="
            (`ISO_8859_2, Mrmime.Encoded_word.b, Ok "u understand the example.");
          make "=?ISO-8859-1?Q?Olle_J=E4rnefors?="
            (`ISO_8859_1, Mrmime.Encoded_word.q, Ok "Olle Järnefors");
          make "=?ISO-8859-1?Q?Patrik_F=E4ltstr=F6m?="
            (`ISO_8859_1, Mrmime.Encoded_word.q, Ok "Patrik Fältström");
          make "=?iso-8859-8?b?7eXs+SDv4SDp7Oj08A==?="
            ( `ISO_8859_8,
              Mrmime.Encoded_word.b,
              Ok "םולש ןב ילטפנ" (* Il est un gentleman *) )
        ] )
    ]
