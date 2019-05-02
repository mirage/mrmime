module Option = struct
  let value_exn = function
    | Some v -> v
    | None -> Fmt.invalid_arg "Option.value_exn"
end

let parse_content_type x =
  let x = x ^ "\r\n" in
  Angstrom.parse_string Mrmime.Rfc2045.content x

let content_type =
  Alcotest.testable Mrmime.Content_type.pp Mrmime.Content_type.equal

let make raw expect =
  Alcotest.test_case raw `Quick
  @@ fun () ->
  match parse_content_type raw with
  | Ok value -> Alcotest.(check content_type) raw expect value
  | Error _ -> Fmt.invalid_arg "Invalid content-type value: %s." raw

let content_type_0 =
  let open Mrmime.Content_type in
  let value =
    let open Rresult.R in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value "us-ascii" >>= fun us_ascii ->
    Subtype.iana Type.text "plain" >>| fun subty ->
    make Type.text subty Parameters.(add charset us_ascii empty) in
  Rresult.R.get_ok value

let content_type_1 =
  let open Mrmime.Content_type in
  let value =
    let open Rresult.R in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value "us-ascii" >>= fun us_ascii ->
    Subtype.iana Type.text "plain" >>| fun subty ->
    make Type.text subty Parameters.(add charset us_ascii empty) in
  Rresult.R.get_ok value

let content_type_2 =
  let open Mrmime.Content_type in
  let value =
    let open Rresult.R in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value (Rosetta.encoding_to_string `ISO_8859_1) >>= fun latin1 ->
    Subtype.iana Type.text "plain" >>| fun subty ->
    make Type.text subty Parameters.(add charset latin1 empty) in
  Rresult.R.get_ok value

let () =
  Alcotest.run "rfc2045"
    [ ( "content-type"
      , [ make "text/plain; charset=us-ascii (Plain text)" content_type_0
        ; make "text/plain; charset=\"us-ascii\"" content_type_1
        ; make "text/plain; charset=ISO-8859-1" content_type_2 ] ) ]
