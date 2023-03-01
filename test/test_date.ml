let ( <.> ) f g x = f (g x)
let date = Alcotest.testable Mrmime.Date.pp Mrmime.Date.equal

let parse_date x =
  let parser =
    let open Angstrom in
    let buf = Bytes.create 0x7f in
    Unstrctrd_parser.unstrctrd buf >>= fun v ->
    let res =
      let ( >>| ) x f = Result.map f x and ( >>= ) = Result.bind in
      Unstrctrd.without_comments v
      >>| Unstrctrd.fold_fws
      >>| Unstrctrd.to_utf_8_string
      >>= (Result.map_error (fun x -> `Msg x)
          <.> Angstrom.parse_string ~consume:Angstrom.Consume.All
                Mrmime.Date.Decoder.date_time)
    in
    match res with Ok v -> return v | Error _ -> fail "Invalid date"
  in
  Angstrom.parse_string ~consume:Angstrom.Consume.All parser (x ^ "\r\n")

let make raw expect =
  Alcotest.test_case (Fmt.str "%S" raw) `Quick @@ fun () ->
  match parse_date raw with
  | Ok value -> Alcotest.(check date) raw expect value
  | Error err -> Fmt.invalid_arg "Invalid date value (%s): %s." err raw

let tests =
  [ ( "Fri, 21 Nov 1997 09:55:06 -0600",
      Mrmime.Date.
        { day = Some Day.Fri;
          date = (21, Month.Nov, 1997);
          time = (9, 55, Some 06);
          zone = Zone.TZ (-06, 00)
        } );
    ( "Tue, 1 Jul 2003 10:52:37 +0200",
      Mrmime.Date.
        { day = Some Day.Tue;
          date = (1, Month.Jul, 2003);
          time = (10, 52, Some 37);
          zone = Zone.TZ (02, 00)
        } );
    ( "Thu, 13 Feb 1969 23:32:54 -0330",
      Mrmime.Date.
        { day = Some Day.Thu;
          date = (13, Month.Feb, 1969);
          time = (23, 32, Some 54);
          zone = Zone.TZ (-03, 30)
        } );
    ( "Mon, 24 Nov 1997 14:22:01 -0800",
      Mrmime.Date.
        { day = Some Day.Mon;
          date = (24, Month.Nov, 1997);
          time = (14, 22, Some 01);
          zone = Zone.TZ (-08, 00)
        } );
    ( "Thu,\r\n\
      \ 13\r\n\
      \   Feb\r\n\
      \     1969\r\n\
      \ 23:32\r\n\
      \          -0330 (Newfoundland Time)",
      Mrmime.Date.
        { day = Some Day.Thu;
          date = (13, Month.Feb, 1969);
          time = (23, 32, None);
          zone = Zone.TZ (-03, 30)
        } );
    ( "21 Nov 97 09:55:06 GMT",
      Mrmime.Date.
        { day = None;
          date = (21, Month.Nov, 97);
          time = (09, 55, Some 06);
          zone = Zone.GMT
        } );
    ( "Fri, 21 Nov 1997 09(comment):   55  :  06 -0600",
      Mrmime.Date.
        { day = Some Day.Fri;
          date = (21, Month.Nov, 1997);
          time = (09, 55, Some 06);
          zone = Zone.TZ (-06, 00)
        } );
    ( "Fri, 21 Nov 1990 00:00:00.1234 -0000",
      Mrmime.Date.
        { day = Some Day.Fri;
          date = (21, Month.Nov, 1990);
          time = (0, 0, Some 0);
          zone = Zone.TZ (0, 0)
        } )
  ]

let () =
  Alcotest.run "date"
    [ ("valid date", List.map (fun (raw, expect) -> make raw expect) tests) ]
