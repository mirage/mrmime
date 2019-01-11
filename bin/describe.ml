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
         let pad = (pc ^(if i = n then "`-" else "|-"),
                    pc ^(if i = n then "  " else "| ")) in
         print_atom ~pad atom)
      parts
  | _ -> assert false

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
  let common = [ "Date", Header.Set.cardinal header.Header.date
               ; "From", Header.Set.cardinal header.Header.from
               ; "Sender", Header.Set.cardinal header.Header.sender
               ; "Reply-To", Header.Set.cardinal header.Header.reply_to
               ; "To", Header.Set.cardinal header.Header.too
               ; "Cc", Header.Set.cardinal header.Header.cc
               ; "Bcc", Header.Set.cardinal header.Header.bcc
               ; "Subject", Header.Set.cardinal header.Header.subject
               ; "Message-ID", Header.Set.cardinal header.Header.message_id
               ; "In-Reply-To", Header.Set.cardinal header.Header.in_reply_to
               ; "References", Header.Set.cardinal header.Header.references
               ; "Comments", Header.Set.cardinal header.Header.comments
               ; "Keywords", Header.Set.cardinal header.Header.keywords ] in
  let fields =
    Header.Set.fold
      (fun i a -> match Ptmap.find (i :> int) header.Header.ordered with
         | Header.B (Header.Field field, _) -> Header.Field.(capitalize (canonicalize field)) :: a
         | _ -> a) header.Header.fields [] |>
    merge in
  let unsafes =
    Header.Set.fold
      (fun i a -> match Ptmap.find (i :> int) header.Header.ordered with
         | Header.B (Header.Unsafe field, _) -> Header.Field.(capitalize (canonicalize field)) :: a
         | _ -> a) header.Header.unsafes [] |>
    merge in
  List.concat [ common; fields; unsafes ]

let pp_header =
  let pp_data ppf (field, value) =
    Fmt.pf ppf "%s: %d" field value in
  Fmt.(hvbox (list ~sep:(always "@\n") pp_data))

let parse_and_print with_header ic =
  let open Rresult.R in
  mail_of_input ic >>| fun (header, mail) ->
  if with_header then Fmt.pr "header: %a.\n%!" pp_header (count header) ;
  print_mail mail

let run with_header input =
  let close, ic =
    match input with
    | `Path x -> let ic = open_in (Fpath.to_string x) in (fun () -> close_in ic), ic
    | `Std -> (fun () -> ()), stdin in
  let v = parse_and_print with_header ic in
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
  Term.(const run $ header $ source), Term.info "describe" ~doc ~exits ~man
