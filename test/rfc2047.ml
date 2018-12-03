let parse_encoded_word x = Angstrom.parse_string Mrmime.Rfc2047.encoded_word x

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
    (Fmt.Dump.result ~ok:Fmt.string ~error:Rresult.R.pp_msg)
    (Rresult.R.equal ~ok:String.equal ~error:(fun (`Msg _) (`Msg _) -> true))

let make ~name raw (expect_charset, expect_encoding, expect_data) =
  Alcotest.test_case name `Quick
  @@ fun () ->
  match parse_encoded_word raw with
  | Ok value ->
      Alcotest.(check charset)
        "charset"
        (Mrmime.Encoded_word.charset value)
        expect_charset ;
      Alcotest.(check encoding)
        "encoding"
        (Mrmime.Encoded_word.encoding value)
        expect_encoding ;
      Alcotest.(check data) "data" (Mrmime.Encoded_word.data value) expect_data
  | Error _ -> Fmt.invalid_arg "Invalid encoded-word: %s." raw

let () =
  Alcotest.run "rfc2047"
    [ ( "encoded-word"
      , [ make ~name:"0" "=?US-ASCII?Q?Keith_Moore?="
            (`US_ASCII, Mrmime.Encoded_word.q, Ok "Keith Moore")
        ; make ~name:"1" "=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?="
            (`ISO_8859_1, Mrmime.Encoded_word.q, Ok "Keld Jørn Simonsen")
        ; make ~name:"2" "=?ISO-8859-1?Q?Andr=E9_?="
            (`ISO_8859_1, Mrmime.Encoded_word.q, Ok "André ")
        ; make ~name:"3" "=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?="
            (`ISO_8859_1, Mrmime.Encoded_word.b, Ok "If you can read this yo")
        ; make ~name:"4"
            "=?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?="
            (`ISO_8859_2, Mrmime.Encoded_word.b, Ok "u understand the example.")
        ; make ~name:"5" "=?ISO-8859-1?Q?Olle_J=E4rnefors?="
            (`ISO_8859_1, Mrmime.Encoded_word.q, Ok "Olle Järnefors")
        ; make ~name:"6" "=?ISO-8859-1?Q?Patrik_F=E4ltstr=F6m?="
            (`ISO_8859_1, Mrmime.Encoded_word.q, Ok "Patrik Fältström")
        ; make ~name:"7" "=?iso-8859-8?b?7eXs+SDv4SDp7Oj08A==?="
            ( `ISO_8859_8
            , Mrmime.Encoded_word.b
            , Ok "םולש ןב ילטפנ" (* Il est un gentleman *) ) ] ) ]
