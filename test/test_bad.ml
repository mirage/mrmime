let contents_of_filename filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

let be_able_to_decode filename =
  Alcotest.test_case filename `Quick @@ fun () ->
  let parser = Mrmime.Mail.mail None in
  let contents = contents_of_filename filename in
  match Angstrom.parse_string ~consume:All parser contents with
  | Ok _ -> Alcotest.(check pass) filename () ()
  | Error _err -> Alcotest.failf "Impossible to parse %s" filename

let emails =
  [ "bad/001.eml";
    "bad/002.eml";
    "bad/003.eml";
    "bad/004.eml";
    "bad/005.eml";
    "bad/006.eml";
    "bad/007.eml";
    "bad/008.eml";
    "bad/009.eml";
    "bad/010.eml";
    "bad/011.eml";
    "bad/012.eml";
    "bad/013.eml";
    "bad/014.eml";
    "bad/015.eml"
  ]

let () =
  Alcotest.run "really bad"
    [ ("really bad", List.map be_able_to_decode emails) ]
