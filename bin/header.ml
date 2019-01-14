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

let pp_field : type a. a Header.field Fmt.t = let open Header in fun ppf -> function
  | Date -> Fmt.string ppf "Date"
  | From -> Fmt.string ppf "From"
  | Sender -> Fmt.string ppf "Sender"
  | ReplyTo -> Fmt.string ppf "Reply-To"
  | To -> Fmt.string ppf "To"
  | Cc -> Fmt.string ppf "Cc"
  | Bcc -> Fmt.string ppf "Bcc"
  | Subject -> Fmt.string ppf "Subject"
  | MessageID -> Fmt.string ppf "Message-ID"
  | InReplyTo -> Fmt.string ppf "In-Reply-To"
  | References -> Fmt.string ppf "References"
  | Comments -> Fmt.string ppf "Comments"
  | Keywords -> Fmt.string ppf "Keywords"
  | Resent -> Fmt.string ppf "Resents"
  | Trace -> Fmt.string ppf "Traces"
  | Field field -> Header.Field.pp ppf field
  | Line -> Fmt.string ppf "#line"
  | Unsafe field -> Header.Field.pp ppf field

type t = Header.value
type binding = Header.binding

type error = [ `Not_found | Rresult.R.msg ]

let get_field : type a. a Header.field -> Header.t -> ((a * Location.t) list, error) result = fun field header ->
  match Header.get field header with
  | [] -> Error `Not_found
  | lst -> Ok lst

let get_fields fields header =
  List.fold_left
    (fun a (Header.V field) -> match a, get_field field header with
       | Ok a, Ok l -> Ok (List.map (fun (v, loc) -> Header.B (field, v, loc)) l @ a)
       | Error _, _ -> a
       | Ok a, Error _ ->
         let field = Header.Unsafe (Fmt.to_to_string pp_field field) in
         match get_field field header with
         | Ok l -> Ok (List.map (fun (v, loc) -> Header.B (field, v, loc)) l @ a)
         | Error e -> Error e)
    (Ok []) fields

let pp_of_field = Header.pp_value_of_field

let ( <.> ) f g = fun x -> f (g x)

let pp_of_binding ppf (Header.B (field, v, loc)) =
  let pp_value = pp_of_field field in
  Fmt.pf ppf "%a[%a]: @[<hov>%a@]" pp_field field Location.pp loc pp_value v

let extract ~newline ic fields =
  let print binding =
    Fmt.pr "%a@\n" pp_of_binding binding in
  let open Rresult.R in
  header_of_input ~newline ic >>= fun (_, header, _) ->
  get_fields fields header >>| List.iter print

let run newline input fields =
  let close, ic =
    match input with
    | `Path x -> let ic = open_in (Fpath.to_string x) in (fun () -> close_in ic), ic
    | `Std -> (fun () -> ()), stdin in
  let v = extract ~newline ic fields in
  close () ; v

open Cmdliner

let field =
  let parser = function
    | "date" -> Header.V Date
    | "from" -> Header.V From
    | "sender" -> Header.V Sender
    | "reply-to" -> Header.V ReplyTo
    | "to" -> Header.V To
    | "cc" -> Header.V Cc
    | "bcc" -> Header.V Bcc
    | "subject" -> Header.V Subject
    | "msg-id" -> Header.V MessageID
    | "in-reply-to" -> Header.V InReplyTo
    | "references" -> Header.V References
    | "comments" -> Header.V Comments
    | "keywords" -> Header.V Keywords
    | "resents" -> Header.V Resent
    | "traces" -> Header.V Trace
    | field -> Header.V (Field field) in
  let pp ppf (Header.V x) = pp_field ppf x in
  let parser = Rresult.R.ok <.> parser in
  Arg.conv ~docv:"<field>" (parser <.> Header.Field.canonicalize, pp)

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

let command =
  let doc = "Header extractor" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Extract fields from a mail" ] in
  Term.(const run $ Common.newline $ source $ fields), Term.info "header" ~doc ~exits ~man
