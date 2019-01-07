open Mrmime

let pp_last ppf = function
  | true -> Fmt.string ppf "`"
  | false -> Fmt.string ppf "|"

let pp_level ~last ppf n =
  let rec go = function
    | 0 -> ()
    | n -> if last then Fmt.string ppf "  " else Fmt.string ppf "| " ; go (pred n) in
  assert (n >= 0) ; go n

let pp_list pp_data ppf lst =
  let rec go = function
    | [] -> ()
    | [ x ] -> pp_data ~last:true ppf x
    | x :: r -> pp_data ~last:false ppf x ; go r in
  go lst

let rec pp_atom ~last ~level ppf { Mail.content; part; _ } =
  let ty = Content.ty content in
  let subty = Content.subty content in
  Fmt.pf ppf "%a%a-<%a:%a>\n" (pp_level ~last) level pp_last last Content_type.Type.pp ty Content_type.Subtype.pp subty ;
  Fmt.option ~none:Fmt.nop (pp_part ~level:(succ level) ~last:true) ppf part

and pp_part ~last ~level ppf = function
  | Mail.Part_discrete _ ->
    Fmt.pf ppf "%a%a-<#discrete>\n" (pp_level ~last) level pp_last last
  | Mail.Part_extension _ ->
    Fmt.pf ppf "%a%a-<#extension>\n" (pp_level ~last) level pp_last last
  | Mail.Part_multipart parts ->
    Fmt.pf ppf "%a%a-<#multipart>\n" (pp_level ~last) level pp_last last ;
    pp_list (pp_atom ~level:(succ level)) ppf parts
  | _ -> assert false

and pp_mail ppf = function
  | Mail.Discrete { content; _ } ->
    let ty = Content.ty content in
    let subty = Content.subty content in
    Fmt.pf ppf "+-<%a:%a>\n" Content_type.Type.pp ty Content_type.Subtype.pp subty
  | Mail.Extension { content; _ } ->
    let ty = Content.ty content in
    let subty = Content.subty content in
    Fmt.pf ppf "+-<extension:%a:%a>\n" Content_type.Type.pp ty Content_type.Subtype.pp subty
  | Mail.Multipart { content; parts; _ } ->
    let ty = Content.ty content in
    let subty = Content.subty content in
    Fmt.pf ppf "+-<%a:%a[%d]>\n" Content_type.Type.pp ty Content_type.Subtype.pp subty (List.length parts);
    pp_list (pp_atom ~level:(succ 0)) ppf parts
  | _ -> assert false

let parser = Mrmime.Mail.mail

let mail_of_input ic =
  let open Angstrom.Buffered in
  let raw = Bytes.create 4096 in
  let rec go = function
    | Partial continue ->
      let len = input ic raw 0 (Bytes.length raw) in
      let res = if len = 0 then continue `Eof else continue (`String (Bytes.sub_string raw 0 len)) in
      go res
    | Done (_, mail) -> Ok mail
    | Fail (_, _, err) ->
      Fmt.epr "Got an error: %s.\n%!" err ;
      Error (`Msg err) in
  go (parse parser)

let parse_and_print ic =
  let open Rresult.R in
  mail_of_input ic >>| fun (_, mail) -> Fmt.pr "%a" pp_mail mail

let run input =
  let close, ic =
    match input with
    | `Path x -> let ic = open_in (Fpath.to_string x) in (fun () -> close_in ic), ic
    | `Std -> (fun () -> ()), stdin in
  let v = parse_and_print ic in
  close () ; v

open Cmdliner

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

let command =
  let doc = "Header extractor" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Extract fields from a mail" ] in
  Term.(const run $ source), Term.info "describe" ~doc ~exits ~man
