module Option = Opt

let parser = Mrmime.Mail.header

let header_of_string = Angstrom.parse_string parser

let header_of_input ic =
  let open Angstrom.Buffered in
  let raw = Bytes.create 4096 in
  let rec go = function
    | Partial continue ->
      let len = input ic raw 0 (Bytes.length raw) in
      let res = if len = 0 then continue `Eof else continue (`String (Bytes.sub_string raw 0 len)) in
      go res
    | Done (_, header) -> Ok header
    | Fail (_, _, err) -> Error (`Msg err) in
  go (parse parser)

open Mrmime

type 'a field =
  | Date : Date.t field
  | From : Mailbox.t list field
  | Sender : Mailbox.t field
  | ReplyTo : Address.t list field
  | To : Address.t list field
  | Cc : Address.t list field
  | Bcc : Address.t list field
  | Subject : Unstructured.t field
  | MsgID : Msg_id.t field
  | InReplyTo : Header.phrase_or_msg_id list field
  | References : Header.phrase_or_msg_id list field
  | Comments : Unstructured.t list field
  | Keywords : Header.phrase list list field
  | Resents : Resent.t list field
  | Traces : Trace.t list field
  | Field : string -> Unstructured.t list field

let pp_field : type a. a field Fmt.t = fun ppf -> function
  | Date -> Fmt.string ppf "date"
  | From -> Fmt.string ppf "from"
  | Sender -> Fmt.string ppf "sender"
  | ReplyTo -> Fmt.string ppf "reply-to"
  | To -> Fmt.string ppf "to"
  | Cc -> Fmt.string ppf "cc"
  | Bcc -> Fmt.string ppf "bcc"
  | Subject -> Fmt.string ppf "subject"
  | MsgID -> Fmt.string ppf "msg-id"
  | InReplyTo -> Fmt.string ppf "in-rpely-to"
  | References -> Fmt.string ppf "references"
  | Comments -> Fmt.string ppf "comments"
  | Keywords -> Fmt.string ppf "keywords"
  | Resents -> Fmt.string ppf "resents"
  | Traces -> Fmt.string ppf "traces"
  | Field field -> Fmt.using String.capitalize_ascii Fmt.string ppf field


type t = V : 'a field -> t
type binding = B : 'a field * 'a -> binding

type error = [ `Not_found | Rresult.R.msg ]

let get_field : type a. a field -> Header.t -> (a, error) result = fun field header ->
  match field with
  | Date -> Opt.to_result ~error:`Not_found header.Header.date
  | From -> Rresult.R.ok header.Header.from
  | Sender -> Opt.to_result ~error:`Not_found header.Header.sender
  | ReplyTo -> Rresult.R.ok header.Header.reply_to
  | To -> Rresult.R.ok header.Header.too
  | Cc -> Rresult.R.ok header.Header.cc
  | Bcc -> Rresult.R.ok header.Header.bcc
  | Subject -> Opt.to_result ~error:`Not_found header.Header.subject
  | MsgID -> Opt.to_result ~error:`Not_found header.Header.msg_id
  | InReplyTo -> Rresult.R.ok header.Header.in_reply_to
  | References -> Rresult.R.ok header.Header.references
  | Comments -> Rresult.R.ok header.Header.comments
  | Keywords -> Rresult.R.ok header.Header.keywords
  | Resents -> Rresult.R.ok header.Header.resents
  | Traces -> Rresult.R.ok header.Header.traces
  | Field field ->
    match Header.Map.find field header.Header.field with
    | v -> Rresult.R.ok v
    | exception Not_found -> Error `Not_found

let get_fields fields header =
  List.fold_left
    (fun a (V field) -> match a, get_field field header with
       | Ok a, Ok v -> Ok (B (field, v) :: a)
       | Error _, _ -> a
       | _, Error e -> Error e)
    (Ok []) fields

let pp_of_field : type a. a field -> a Fmt.t = function
  | Date -> Date.pp
  | From -> Fmt.Dump.list Mailbox.pp
  | Sender -> Mailbox.pp
  | ReplyTo -> Fmt.Dump.list Address.pp
  | To -> Fmt.Dump.list Address.pp
  | Cc -> Fmt.Dump.list Address.pp
  | Bcc -> Fmt.Dump.list Address.pp
  | Subject -> Unstructured.pp
  | MsgID -> Msg_id.pp
  | InReplyTo -> Fmt.Dump.list Header.pp_phrase_or_msg_id
  | References -> Fmt.Dump.list Header.pp_phrase_or_msg_id
  | Comments -> Fmt.Dump.list Unstructured.pp
  | Keywords -> Fmt.Dump.list (Fmt.Dump.list Header.pp_phrase)
  | Resents -> Fmt.Dump.list Resent.pp
  | Traces -> Fmt.Dump.list Trace.pp
  | Field field -> fun ppf x -> Fmt.(Dump.pair string (Dump.list Unstructured.pp)) ppf (field, x)

let ( <.> ) f g = fun x -> f (g x)

let pp_of_binding ppf (B (field, v)) =
  let pp_value = pp_of_field field in
  Fmt.pf ppf "@[<hov>%a:@ %a@]" pp_field field pp_value v

let extract ic fields =
  let print binding =
    Fmt.pr "%a@\n" pp_of_binding binding in
  let open Rresult.R in
  header_of_input ic >>= fun (_, header, _) ->
  get_fields fields header >>| List.iter print

let run input fields =
  let close, ic =
    match input with
    | `Path x -> let ic = open_in (Fpath.to_string x) in (fun () -> close_in ic), ic
    | `Std -> (fun () -> ()), stdin in
  let v = extract ic fields in
  close () ; v

open Cmdliner

let field =
  let parser = function
    | "date" -> V Date
    | "from" -> V From
    | "sender" -> V Sender
    | "reply-to" -> V ReplyTo
    | "to" -> V To
    | "cc" -> V Cc
    | "bcc" -> V Bcc
    | "subject" -> V Subject
    | "msg-id" -> V MsgID
    | "in-reply-to" -> V InReplyTo
    | "references" -> V References
    | "comments" -> V Comments
    | "keywords" -> V Keywords
    | "resents" -> V Resents
    | "traces" -> V Traces
    | field -> V (Field field) in
  let pp ppf (V x) = pp_field ppf x in
  let parser = Rresult.R.ok <.> parser in
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

let command =
  let doc = "Header extractor" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Extract fields from a mail" ] in
  Term.(const run $ source $ fields), Term.info "header" ~doc ~exits ~man
