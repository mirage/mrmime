let random_bytes bytes =
  for i = 0 to Bytes.length bytes - 1 do
    Bytes.set bytes i (Char.unsafe_chr (Random.int 256))
  done

let random_7bits bytes =
  for i = 0 to Bytes.length bytes - 1 do
    Bytes.set bytes i (Char.chr (Random.int 128))
  done

let generate_any random =
  ();
  fun len max ->
    let tmp = Bytes.create len in
    let pos = ref 0 in
    fun () ->
      if !pos >= max then None
      else (
        random tmp;
        let len = min (max - !pos) len in
        pos := !pos + len;
        Some (Bytes.unsafe_to_string tmp, 0, len))

let generate_any_bytes len max = generate_any random_bytes len max
let generate_any_7bits len max = generate_any random_7bits len max

let generate_any_bytes_with_newline len max =
  let g = generate_any random_bytes len max in
  fun () ->
    match g () with
    | Some (str, off, len) ->
        let buf' = Bytes.create (len + 1) in
        Bytes.blit_string str off buf' 0 len;
        Bytes.set buf' len '\n';
        Some (Bytes.unsafe_to_string buf', 0, len + 1)
    | None -> None

open Mrmime

let max = 10_000
let chunk = 0x1000

let part content_encoding max =
  let content_type =
    Content_type.(make `Text (Subtype.v `Text "plain") Parameters.empty)
  in
  let header =
    Header.empty
    |> Header.add Field_name.content_type (Field.Content, content_type)
    |> Header.add Field_name.content_encoding (Field.Encoding, content_encoding)
  in
  let generate =
    match content_encoding with
    | `Base64 | `Binary | `Bit8 | `Quoted_printable -> generate_any_bytes
    | `Bit7 | _ -> generate_any_7bits
  in
  Mt.part ~header (generate chunk max)

let mail =
  let part0 = part `Bit7 max in
  let part1 = part `Quoted_printable max in
  let part2 = part `Base64 max in
  Mt.make Header.empty Mt.multi
    (Mt.multipart ~rng:Mt.rng ~boundary:"boundary" [ part0; part1; part2 ])

let fully_write oc str off len = output_substring oc str off len

let rec show stream =
  match stream () with
  | Some (buf, off, len) ->
      fully_write stdout buf off len;
      show stream
  | None -> ()

let rec drain stream =
  match stream () with Some _ -> drain stream | None -> ()

let regenerate part =
  let part0 = generate_any_7bits chunk max in
  let part1 = generate_any_bytes_with_newline chunk max in
  let part2 = generate_any_bytes chunk max in
  match part with
  | 0 -> show part0
  | 1 ->
      drain part0;
      show part1
  | 2 ->
      drain part0;
      drain part1;
      show part2
  | _ ->
      Fmt.epr "%s [<seed>] [<part>]: Invalid unique ID of part.\n%!"
        Sys.argv.(0)

let random_init_with_seed seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  Random.full_init res

let () =
  match Sys.argv with
  | [| _; seed |] -> (
      try
        let seed = Base64.decode_exn seed in
        random_init_with_seed seed;
        show (Mt.to_stream mail)
      with _ -> Fmt.epr "%s [<seed>] [<part>]\n%!" Sys.argv.(0))
  | [| _; seed; part |] -> (
      try
        let seed = Base64.decode_exn seed in
        let part = int_of_string part in
        random_init_with_seed seed;
        regenerate part
      with _ -> Fmt.epr "%s [<seed>] [<part>]\n%!" Sys.argv.(0))
  | _ ->
      Random.self_init ();
      show (Mt.to_stream mail)
