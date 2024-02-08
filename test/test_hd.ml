open Mrmime

let parsers =
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

let test_000 =
  {|Date:     26 Aug 76 14:29 EDT|}^"\r"^{|
From:     Jones@Registry.Org|}^"\r"^{|
Bcc:|}^"\r"^{|
|}^"\r"^{|
|}

module Map = Map.Make (Field_name)

let to_unstrctrd (unstructured : Unstructured.t) =
  let fold acc = function #Unstrctrd.elt as elt -> elt :: acc | _ -> acc in
  List.fold_left fold [] unstructured
  |> List.rev
  |> Unstrctrd.of_list
  |> Result.get_ok

let add k v m =
  try
    let vs = Map.find k m in
    Map.add k (v :: vs) m
  with Not_found -> Map.add k [ v ] m

let parse str =
  let pos = ref 0 in
  let decoder = Hd.decoder parsers in
  let rec go acc =
    match Hd.decode decoder with
    | `End prelude ->
        Alcotest.(check string) "prelude" prelude "";
        acc
    | `Field field -> (
        let (Field.Field (field_name, w, v)) = Location.prj field in
        match w with
        | Field.Unstructured ->
            let v = Unstrctrd.(to_utf_8_string (fold_fws (to_unstrctrd v))) in
            go (add field_name v acc)
        | _ -> assert false)
    | `Malformed err -> Alcotest.failf "Hd.decode: %s" err
    | `Await ->
        let len = min (String.length str - !pos) 0x100 in
        Hd.src decoder str !pos len;
        pos := !pos + len;
        go acc
  in
  go Map.empty

let test_000 =
  Alcotest.test_case "header-000" `Quick @@ fun () ->
  let fields = parse test_000 in
  Alcotest.(check (list string))
    "Date"
    (Map.find Field_name.date fields)
    [ "     26 Aug 76 14:29 EDT" ];
  Alcotest.(check (list string))
    "From"
    (Map.find Field_name.from fields)
    [ "     Jones@Registry.Org" ];
  Alcotest.(check (list string)) "Bcc" (Map.find Field_name.bcc fields) [ "" ]

let test_001 =
  {|From  : John Doe <jdoe@machine(comment).  example>|}^"\r"^{|
To    : Mary Smith|}^"\r"^{|
  |}^"\r"^{|
          <mary@example.net>|}^"\r"^{|
Subject     : Saying Hello|}^"\r"^{|
Date  : Fri, 21 Nov 1997 09(comment):   55  :  06 -0600|}^"\r"^{|
Message-ID  : <1234   @   local(blah)  .machine .example>|}^"\r"^{|
|}^"\r"^{|
|}

let test_001 =
  Alcotest.test_case "header-000" `Quick @@ fun () ->
  let fields = parse test_001 in
  Alcotest.(check (list string))
    "From"
    (Map.find Field_name.from fields)
    [ " John Doe <jdoe@machine(comment).  example>" ];
  Alcotest.(check (list string))
    "To"
    (Map.find (Field_name.v "To") fields)
    [ " Mary Smith  <mary@example.net>" ];
  Alcotest.(check (list string))
    "Subhect"
    (Map.find Field_name.subject fields)
    [ " Saying Hello" ];
  Alcotest.(check (list string))
    "Date"
    (Map.find Field_name.date fields)
    [ " Fri, 21 Nov 1997 09(comment):   55  :  06 -0600" ];
  Alcotest.(check (list string))
    "Message-ID"
    (Map.find Field_name.message_id fields)
    [ " <1234   @   local(blah)  .machine .example>" ]

let () = Alcotest.run "hd" [ ("header", [ test_000; test_001 ]) ]
