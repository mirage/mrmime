open Crowbar
open Mrmime

let space = map [ range ~min:1 4 ] Unstructured_with_encoded.Craft.sp

let ascii_word =
  map
    [ dynamic_bind (range 78) Common.(string_from_alphabet atext) ]
    Unstructured_with_encoded.Craft.v

let encoded_word =
  map [ bool; list1 uchar ] @@ fun base64 input ->
  let input =
    let buf = Buffer.create (2 * List.length input) in
    List.iter (Uutf.Buffer.add_utf_8 buf) input;
    Buffer.contents buf
  in
  match base64 with
  | true ->
      Unstructured_with_encoded.Craft.e ~encoding:Mrmime.Encoded_word.b input
  | false ->
      Unstructured_with_encoded.Craft.e ~encoding:Mrmime.Encoded_word.q input

let unstructured_with_encoded =
  map [ list (choose [ ascii_word; encoded_word; space ]) ] @@ fun data ->
  Unstructured_with_encoded.Craft.compile data

let () =
  add_test ~name:"unstructured_with_encoded" [ unstructured_with_encoded ]
  @@ fun input ->
  let encoder = Unstructured_with_encoded.Encoder.unstructured_with_encoded in
  let input_str = Prettym.to_string ~new_line:"\r\n " encoder input in
  let reparsed =
    let decoder =
      Unstructured_with_encoded.Decoder.unstructured_with_encoded ()
    in
    Angstrom.parse_string ~consume:All decoder (input_str ^ "\r\n")
    |> Result.fold ~ok:Fun.id ~error:(Crowbar.failf "parse failed: %s")
  in
  let reparsed_str =
    Prettym.to_string ~margin:max_int ~new_line:"\r\n" encoder reparsed
  in
  (* Restort to comparing the rendered output, since we don't have equality or
   * fully normalized representation. *)
  check_eq
    ~pp:Format.(fun ppf -> fprintf ppf "%S")
    ~eq:String.equal input_str reparsed_str
