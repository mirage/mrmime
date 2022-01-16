let parse_msg_id x =
  Angstrom.parse_string ~consume:Angstrom.Consume.All
    Mrmime.MessageID.Decoder.message_id x

let msg_id = Alcotest.testable Mrmime.MessageID.pp Mrmime.MessageID.equal

let make raw expect =
  Alcotest.test_case raw `Quick @@ fun () ->
  match parse_msg_id raw with
  | Ok value -> Alcotest.(check msg_id) raw expect value
  | Error _ -> Fmt.invalid_arg "Invalid msg-id value: %s." raw

let tests : (string * Mrmime.MessageID.t) list =
  [ ( "<1234@local.machine.example>",
      ([ `Atom "1234" ], `Domain [ "local"; "machine"; "example" ]) );
    ( "<5678.21-Nov-1997@example.com>",
      ([ `Atom "5678"; `Atom "21-Nov-1997" ], `Domain [ "example"; "com" ]) );
    ( "<testabcd.1234@silly.example>",
      ([ `Atom "testabcd"; `Atom "1234" ], `Domain [ "silly"; "example" ]) );
    ("<3456@example.net>", ([ `Atom "3456" ], `Domain [ "example"; "net" ]));
    ( "<abcd.1234@local.machine.tld>",
      ([ `Atom "abcd"; `Atom "1234" ], `Domain [ "local"; "machine"; "tld" ]) );
    ("<78910@example.net>", ([ `Atom "78910" ], `Domain [ "example"; "net" ]));
    ( "             <testabcd.1234@silly.test>",
      ([ `Atom "testabcd"; `Atom "1234" ], `Domain [ "silly"; "test" ]) );
    ( "<1234   @   local(blah)  .machine .example>",
      ([ `Atom "1234" ], `Domain [ "local"; "machine"; "example" ]) );
    ( "<089e01493ca6f216ca04fafe7e67@google.com>",
      ([ `Atom "089e01493ca6f216ca04fafe7e67" ], `Domain [ "google"; "com" ]) );
    ( "<CAL4csrQ8JPJ+7MMrzn6wOTC8rPxOTdLoUnQz+MPDCHTuebDTOA@mail.gmail.com>",
      ( [ `Atom "CAL4csrQ8JPJ+7MMrzn6wOTC8rPxOTdLoUnQz+MPDCHTuebDTOA" ],
        `Domain [ "mail"; "gmail"; "com" ] ) );
    ( "<mirage/irmin/pull/378/c259513470@github.com>",
      ([ `Atom "mirage/irmin/pull/378/c259513470" ], `Domain [ "github"; "com" ])
    )
  ]

let make_output v expect =
  Alcotest.test_case (Fmt.to_to_string Mrmime.MessageID.pp v) `Quick
  @@ fun () ->
  let res = Prettym.to_string Mrmime.MessageID.Encoder.message_id v in
  Alcotest.(check string) "result" res expect

let tests_caml : (Mrmime.MessageID.t * string) list =
  let open Mrmime in
  [ ( ( Mailbox.Local.(v [ w "FE47A9B" ]),
        MessageID.Domain.(v domain [ a "gmail"; a "com" ]) ),
      "<FE47A9B@gmail.com>" )
  ]

let () =
  Alcotest.run "msg-id"
    [ ("valid msg-id", List.map (fun (raw, expect) -> make raw expect) tests);
      ("output", List.map (fun (v, expect) -> make_output v expect) tests_caml)
    ]
