let parse_obs_unstruct x =
  let x = x ^ "\r\n\r\n" in
  Angstrom.parse_string Mrmime.Rfc5322.obs_unstruct x

let unstructured =
  let unstructured_atom_eq a b =
    match (a, b) with
    | `Text a, `Text b -> String.equal a b
    | `CR a, `CR b -> a = b
    | `LF a, `LF b -> a = b
    | `CRLF, `CRLF -> true
    | `WSP, `WSP -> true
    | `Encoded a, `Encoded b -> Mrmime.Encoded_word.equal a b
    | _, _ -> false
  in
  let unstructured_eq a b = try List.for_all2 unstructured_atom_eq a b with _ -> false in
  let pp_unstructured_atom ppf = function
    | `Text x -> Fmt.hvbox Mrmime.Utils.pp_string ppf x
    | `CR n -> Fmt.pf ppf "<cr:%d>" n
    | `LF n -> Fmt.pf ppf "<lf:%d>" n
    | `CRLF -> Fmt.pf ppf "<crlf>"
    | `WSP -> Fmt.pf ppf "<wsp>"
    | `Encoded x -> Fmt.hvbox Mrmime.Encoded_word.pp ppf x
  in
  let pp_unstructured = Fmt.Dump.list pp_unstructured_atom in
  Alcotest.testable pp_unstructured unstructured_eq

let make_valid raw expect =
  Alcotest.test_case (Fmt.to_to_string Mrmime.Utils.pp_string raw) `Quick
  @@ fun () ->
  match parse_obs_unstruct raw with
  | Ok value -> Alcotest.(check unstructured) raw expect value
  | Error _ -> Fmt.invalid_arg "Invalid unstructured value: %s." raw

let () =
  Alcotest.run "obs-unstruct"
    [("valid obs-unstruct value", [ make_valid "" []
                                  ; make_valid "\r" [ `CR 1 ]
                                  ; make_valid "\r\r" [ `CR 2 ]
                                  ; make_valid "\r\n \r" [ `CRLF; `WSP; `CR 1 ]
                                  ; make_valid "\n" [ `LF 1 ]
                                  ; make_valid "\n\n\r\n " [ `LF 2; `CRLF; `WSP ]
                                  ; make_valid "text" [ `Text "text" ]] ) ]
