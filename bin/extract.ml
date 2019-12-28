open Common

let run ~newline ic capacity =
  let open Mrmime in
  let open Hd in
  let raw = Bytes.create capacity in
  let buffer = Bigstringaf.create (2 * capacity) in
  let decoder = decoder buffer in
  let rec go () = match decode decoder with
    | `Field v ->
      let v = Location.prj v in
      Fmt.pr "%a@\n%!" Field.pp v ; go ()
    | `Malformed err ->Rresult.R.error_msg err
    | `End _ -> Rresult.R.ok ()
    | `Await ->
      let len = input ic raw 0 capacity in
      let raw = sanitize_input newline raw len in
      match src decoder raw 0 (String.length raw) with
      | Ok () -> go ()
      | Error _ as err -> err in
  go ()

let run newline input capacity =
  let close, ic =
    match input with
    | `Path x -> let ic = open_in (Fpath.to_string x) in (fun () -> close_in ic), ic
    | `Std -> (fun () -> ()), stdin in
  let res = run ~newline ic capacity in
  close () ; (res :> (unit, Header.error) result)

open Cmdliner

let field =
  let open Mrmime in
  let parser = Field_name.of_string in
  let pp = Field_name.pp in
  Arg.conv (parser, pp)

let field = Arg.(required & opt (some field) None & info [ "f"; "field" ] ~doc:"field to extract")
let filename = Header.filename
let source = Header.source

let[@inline always] is_power_of_two v = v <> 0 && v land (lnot v + 1) = v

let capacity =
  let parser x =
    try let x = int_of_string x in
      if is_power_of_two x
      then Ok x
      else Rresult.R.error_msgf "%d must be a power of two" x
    with _ -> Rresult.R.error_msgf "Invalid number %s" x in
  let pp = Fmt.int in
  Arg.conv (parser, pp)

let capacity = Arg.(value & opt capacity 0x1000 & info [ "c"; "capacity" ] ~doc:"capacity of the ring-buffer")

let command =
  let doc = "Field extractor" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Extract one field in one pass (safe to use standard input)" ] in
  Term.(const run $ Common.newline $ source $ capacity),
  Term.info "extract" ~doc ~exits ~man
