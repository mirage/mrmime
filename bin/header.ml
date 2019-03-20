module Option = Opt
open Common

let parser = Mrmime.Mail.header

let header_of_string = Angstrom.parse_string parser

let header_of_input ~newline ic =
  let open Angstrom.Buffered in
  let raw = Bytes.create 4096 in
  let rec go = function
    | Partial continue ->
      let len = input ic raw 0 (Bytes.length raw) in
      let raw = sanitize_input newline raw len in
      let res = if len = 0 then continue `Eof else continue (`String raw) in
      go res
    | Done (_, header) -> Ok header
    | Fail (_, _, err) -> Error (`Msg err) in
  go (parse parser)

open Mrmime

let pp_field
  : type a. a Header.field Fmt.t
  = fun ppf field -> Fmt.of_to_string Header.field_to_string ppf field

type error = [ `Not_found | Rresult.R.msg ]

let get_field : Field.t -> Header.t -> (Header.Value.t list, error) result = fun field header ->
  match Header.get field header with
  | [] -> Error `Not_found
  | lst -> Ok lst

let get_fields fields header =
  List.fold_left
    (fun a field -> match a, get_field field header with
       | Ok a, Ok l -> Ok (l @ a)
       | Error _, _ -> a
       | Ok _, Error e -> Error e)
    (Ok []) fields

let ( <.> ) f g = fun x -> f (g x)

let extract_raw ic (Header.B (_, _, loc)) =
  (* TODO: lie when input use LF as line-breaker. *)
  let old = pos_in ic in
  seek_in ic (Location.left_exn loc) ;
  let res = really_input_string ic (Location.length_exn loc) in
  seek_in ic old ; res

let extract ~newline ~with_raw ic fields =
  let print ~with_raw v =
    Fmt.pr "%a@\n@\n" pp_of_binding binding ;
    if with_raw then Fmt.pr "%a@\n" Utils.pp_string (extract_raw ic binding) in
  let open Rresult.R in
  header_of_input ~newline ic >>= fun (_, header, _) ->
  get_fields fields header >>| List.rev >>| List.iter (print ~with_raw)

let run newline with_raw input fields =
  let close, ic =
    match input with
    | `Path x -> let ic = open_in (Fpath.to_string x) in (fun () -> close_in ic), ic
    | `Std -> (fun () -> ()), stdin in
  let v = extract ~newline ~with_raw ic fields in
  close () ; v

open Cmdliner

let field =
  let parser = Field.of_string in
  let pp = Field.pp in
  Arg.conv ~docv:"<field>" (parser, pp)

let fields =
  Arg.(value & opt (list field) [] & info [ "f"; "fields" ] ~doc:"fields to extract")

let filename =
  let parser x = match Fpath.of_string x with
    | Ok x -> Ok (`Path x)
    | Error _ as err -> err in
  let pp ppf = function
    | `Path x -> Fpath.pp ppf x
    | `Std -> Fmt.string ppf "<input>" in
  Arg.conv (parser, pp)

let source =
  let doc = "Input." in
  Arg.(value & opt filename `Std & info [ "i"; "input" ] ~docv:"<input>" ~doc)

let with_raw =
  let doc = "Print raw slice of field." in
  Arg.(value & flag & info [ "r"; "raw" ] ~doc)

let command =
  let doc = "Header extractor" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Extract fields from a mail" ] in
  Term.(const run $ Common.newline $ with_raw $ source $ fields), Term.info "header" ~doc ~exits ~man
