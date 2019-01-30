open Common
open Mrmime

let rec print_atom ?(pad : string * string = "", "") { Mail.content; part; _ } =
  let ty = Content.ty content in
  let subty = Content.subty content in
  let pd, pc = pad in
  Fmt.pr "%s<%a:%a>\n" pd Content_type.Type.pp ty Content_type.Subtype.pp subty ;
  match part with
  | Some part ->
    print_part ~pad:(pc ^ "`-", "  ") part
  | None -> ()

and print_part ?(pad : string * string = "", "") = function
  | Mail.Part_discrete _ ->
    Fmt.pr "%s<#discrete>\n" (fst pad)
  | Mail.Part_extension _ ->
    Fmt.pr "%s<#extension>\n" (fst pad)
  | Mail.Part_multipart parts ->
    let _, pc = pad in
    let n = List.length parts in
    List.iteri
      (fun i atom ->
         let pad = (pc ^ (if i = n then "`-" else "|-"),
                    pc ^ (if i = n then "  " else "| ")) in
         print_atom ~pad atom)
      parts
  | Mail.Part_message { message; _ }->
    let pd, pc = pad in
    let pad = (pc ^ "`-", pc ^ "  ") in
    Fmt.pr "%s<#message>\n" pd ;
    print_mail ~pad message

and print_mail ?(pad : string * string = "", "") mail =
  let pd, pc = pad in
  match mail with
  | Mail.Discrete { content; _ } ->
    let ty = Content.ty content in
    let subty = Content.subty content in
    Fmt.pr "%s<%a:%a>\n" pd Content_type.Type.pp ty Content_type.Subtype.pp subty
  | Mail.Extension { content; _ } ->
    let ty = Content.ty content in
    let subty = Content.subty content in
    Fmt.pr "%s<extension:%a:%a>\n" pd Content_type.Type.pp ty Content_type.Subtype.pp subty
  | Mail.Multipart { content; parts; _ } ->
    let ty = Content.ty content in
    let subty = Content.subty content in
    Fmt.pr "%s<%a:%a[%d]>\n" pd Content_type.Type.pp ty Content_type.Subtype.pp subty (List.length parts);
    let n = List.length parts - 1 in
    List.iteri
      (fun i atom ->
         let pad = (pc ^ (if i = n then "`-" else "|-"),
                    pc ^ (if i = n then "  " else "| ")) in
         print_atom ~pad atom)
      parts
  | Mail.Message { message; _ } ->
    let pd, pc = pad in
    let pad = (pc ^ "`-", pc ^ "  ") in
    Fmt.pr "%s<#message>\n" pd ;
    print_mail ~pad message

let parser = Mrmime.Mail.mail

let mail_of_input ~newline ic =
  let open Angstrom.Buffered in
  let raw = Bytes.create 4096 in
  let rec go = function
    | Partial continue ->
      let len = input ic raw 0 (Bytes.length raw) in
      let raw = sanitize_input newline raw len in
      let res = if len = 0 then continue `Eof else continue (`String raw) in
      go res
    | Done (_, mail) -> Ok mail
    | Fail (_, _, err) ->
      Fmt.epr "Got an error: %s.\n%!" err ;
      Error (`Msg err) in
  go (parse parser)

module Opt = struct
  type 'a t = 'a option

  let to_int = function
    | Some _ -> 1
    | None -> 0

  let bind_default ~default f = function
    | Some x -> f x
    | None -> default
end

let count header =
  let merge lst =
    let tbl = Hashtbl.create 16 in
    List.iter (fun field -> match Hashtbl.find tbl field with
        | n -> Hashtbl.replace tbl field (succ n)
        | exception Not_found -> Hashtbl.add tbl field 1)
      lst ;
    Hashtbl.fold (fun field v a -> (field, v) :: a) tbl [] in
  let open Mrmime in
  let common = [ "Date", List.length Header.(get date header)
               ; "From", List.length Header.(get from header)
               ; "Sender", List.length Header.(get sender header)
               ; "Reply-To", List.length Header.(get reply_to header)
               ; "To", List.length Header.(get too header)
               ; "Cc", List.length Header.(get cc header)
               ; "Bcc", List.length Header.(get bcc header)
               ; "Subject", List.length Header.(get subject header)
               ; "Message-ID", List.length Header.(get message_id header)
               ; "In-Reply-To", List.length Header.(get in_reply_to header)
               ; "References", List.length Header.(get references header)
               ; "Comments", List.length Header.(get comments header)
               ; "Keywords", List.length Header.(get keywords header) ] in
  let fields = Header.get_fields header |> List.map fst |> List.map fst |> merge in
  let unsafes = Header.get_unsafes header |> List.map fst |> List.map fst |> merge in
  List.concat [ common; fields; unsafes ]

let pp_header =
  let pp_data ppf (field, value) =
    Fmt.pf ppf "%s: %d" field value in
  Fmt.(hvbox (list ~sep:(always "@\n") pp_data))

let parse_and_print ~newline with_header ic =
  let open Rresult.R in
  mail_of_input ~newline ic >>| fun (header, mail) ->
  if with_header then Fmt.pr "header: %a.\n%!" pp_header (count header) ;
  print_mail mail

let run newline with_header input =
  let close, ic =
    match input with
    | `Path x -> let ic = open_in (Fpath.to_string x) in (fun () -> close_in ic), ic
    | `Std -> (fun () -> ()), stdin in
  let v = parse_and_print ~newline with_header ic in
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

let header =
  let doc = "Header." in
  Arg.(value & flag & info [ "h"; "header" ] ~doc)

let command =
  let doc = "Header extractor" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Extract fields from a mail" ] in
  Term.(const run $ Common.newline $ header $ source), Term.info "describe" ~doc ~exits ~man
