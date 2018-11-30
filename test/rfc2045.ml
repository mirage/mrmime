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

let make ~name raw expect =
  Alcotest.test_case name `Quick
  @@ fun () ->
  match parse_content_type raw with
  | Ok value -> Alcotest.(check content_type) raw expect value
  | Error _ -> Fmt.invalid_arg "Invalid content-type value: %s." raw

let content_type_0 =
  let open Mrmime.Content_type in
  let charset = Option.value_exn (Parameters.key "charset") in
  let us_ascii = Option.value_exn (Parameters.value "us-ascii") in
  make Type.text
    Subtype.(Option.value_exn (iana Type.text "plain"))
    Parameters.(add charset us_ascii empty)

let content_type_1 =
  let open Mrmime.Content_type in
  let charset = Option.value_exn (Parameters.key "charset") in
  let us_ascii = Option.value_exn (Parameters.value "us-ascii") in
  make Type.text
    Subtype.(Option.value_exn (iana Type.text "plain"))
    Parameters.(add charset us_ascii empty)

let content_type_2 =
  let open Mrmime.Content_type in
  let charset = Option.value_exn (Parameters.key "charset") in
  let us_ascii =
    Option.value_exn
      (Parameters.value (Rosetta.encoding_to_string `ISO_8859_1))
  in
  make Type.text
    Subtype.(Option.value_exn (iana Type.text "plain"))
    Parameters.(add charset us_ascii empty)

let () =
  Alcotest.run "rfc2045"
    [ ( "content-type"
      , [ make ~name:"0" "text/plain; charset=us-ascii (Plain text)"
            content_type_0
        ; make ~name:"1" "text/plain; charset=\"us-ascii\"" content_type_1
        ; make ~name:"2" "text/plain; charset=ISO-8859-1" content_type_2 ] ) ]
