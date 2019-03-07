open Common

let print_field value v =
  Fmt.pr "Retrieve: @[<hov>%a@].\n%!" (Mrmime.St_header.Value.pp_of_value value) v

let print_others field others =
  let open Mrmime in
  Fmt.pr "Your requested field %a does not exists but you can extract:@\n" Mrmime.Field.pp field ;
  Fmt.pr "%a.\n%!" Fmt.(Dump.hashtbl Field.pp int) others

let run ~newline ic field (Mrmime.St_header.Value.V value) capacity =
  let open Mrmime in
  let open St_header in
  let raw = Bytes.create capacity in
  let buffer = Bigstringaf.create (2 * capacity) in
  let decoder = decoder ~field value buffer in
  let exists = ref false in
  let others = Hashtbl.create 0x10 in
  let add_other field =
    try Hashtbl.add others field (Hashtbl.find others field + 1)
    with Not_found -> Hashtbl.add others field 1 in
  let rec go () = match decode decoder with
    | `Field v ->
      exists := true ; print_field value v ; go ()
    | `Other (Value.B (field', _, _, _)) ->
      add_other field' ; go ()
    | `Other (Value.L (_, _)) -> go ()
    | `Malformed err ->Rresult.R.error_msg err
    | `End ->
      if not !exists then print_others field others ;
      Rresult.R.ok ()
    | `Await ->
      let len = input ic raw 0 capacity in
      let raw = sanitize_input newline raw len in
      match src decoder raw 0 (String.length raw) with
      | Ok () -> go ()
      | Error _ as err -> err in
  go ()

let run newline input field value capacity =
  let close, ic =
    match input with
    | `Path x -> let ic = open_in (Fpath.to_string x) in (fun () -> close_in ic), ic
    | `Std -> (fun () -> ()), stdin in
  let res = run ~newline ic field value capacity in
  close () ; (res :> (unit, Header.error) result)

open Cmdliner

let field =
  let open Mrmime in
  let parser = Field.of_string in
  let pp = Field.pp in
  Arg.conv (parser, pp)

let field = Arg.(required & opt (some field) None & info [ "f"; "field" ] ~doc:"field to extract")
let filename = Header.filename
let source = Header.source

let kind =
  let open Mrmime in
  let parser = St_header.Value.of_string in
  let pp = St_header.Value.pp in
  Arg.conv (parser, (fun ppf (St_header.Value.V x) -> pp ppf x))

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

let value = Arg.(required & opt (some kind) None & info [ "v"; "value" ] ~doc:"kind of value expected")
let capacity = Arg.(value & opt capacity 0x1000 & info [ "c"; "capacity" ] ~doc:"capacity of the ring-buffer")

let command =
  let doc = "Field extractor" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Extract one field in one pass (safe to use standard input)" ] in
  Term.(const run $ Common.newline $ source $ field $ value $ capacity),
  Term.info "extract" ~doc ~exits ~man
