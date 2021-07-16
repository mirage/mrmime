(** Saving mails function *)
let generate_filename dir =
  let time = Unix.gettimeofday () in
  let millisec = (time -. floor time) *. 1000000. |> floor |> Int.of_float in
  let time = time |> floor |> Int.of_float in
  dir ^ "/" ^ Int.to_string time ^ Int.to_string millisec

let build_oc dst =
  match dst with
  | `Standard -> (stdout, ignore)
  | `Dir dir ->
      let oc = open_out (Fpath.to_string dir |> generate_filename) in
      (oc, close_out)

let print dst str =
  let oc, oc_close = build_oc dst in
  output_string oc str;
  oc_close oc

open Mrmime
(** Mail_to_mt conversion function *)

let stream_of_string str : Mt.buffer Mt.stream =
  let consumed = ref false in
  fun () ->
    match !consumed with
    | true -> None
    | false ->
        consumed := true;
        Some (str, 0, String.length str)

let empty_stream () = None

let mail_to_mt (mail : Header.t * string Mail.t) : Mt.t =
  let rec to_part = function
    | header, Mail.Leaf body -> Mt.part ~header (stream_of_string body)
    | header, Message (h, b) ->
        to_mail (h, b) |> Mt.to_stream |> Mt.part ~header
    | header, Multipart parts ->
        let parts : Mt.part list =
          List.map
            (fun (h, m) ->
              match m with
              | Some m -> to_part (h, m)
              | None -> Mt.part ~header:h empty_stream)
            parts
        in
        Mt.multipart ~header ~rng:Mt.rng parts |> Mt.multipart_as_part
  and to_mail = function
    | header, Mail.Leaf body ->
        Mt.part ~header (stream_of_string body)
        |> Mt.make Header.empty Mt.simple
    | header, Message (h, b) ->
        to_mail (h, b)
        |> Mt.to_stream
        |> Mt.part ~header
        |> Mt.make Header.empty Mt.simple
    | header, Multipart parts ->
        let parts : Mt.part list =
          List.map
            (fun (h, m) ->
              match m with
              | Some m -> to_part (h, m)
              | None -> Mt.part ~header:h empty_stream)
            parts
        in
        Mt.multipart ~header ~rng:Mt.rng parts |> Mt.make Header.empty Mt.multi
  in
  to_mail mail

(** *_to_string and printing (on stdout) functions, mainly for debugging *)
let buffer_stream_to_string v =
  let buf = Buffer.create 0x1000 in
  let rec go () =
    match v () with
    | Some (str, off, len) ->
        Buffer.add_substring buf str off len;
        go ()
    | None -> Buffer.contents buf
  in
  go ()

let stream_to_string v =
  let rec go acc () =
    match v () with Some str -> go (str :: acc) () | None -> acc
  in
  String.concat "" (List.rev (go [] ()))

let is_a_header line =
  String.split_on_char ':' line |> function
  | [] | [ _ ] -> false
  | _ :: y :: _ -> String.get y 0 = ' '

let count_header h =
  let lines =
    Header.to_stream h
    |> stream_to_string
    |> String.split_on_char '\n'
    |> List.filter is_a_header
  in
  List.length lines

let headers_count_to_string h = (count_header h |> string_of_int) ^ ", "

let rec struct_to_string = function
  | hp, Mail.Leaf s ->
      headers_count_to_string hp
      ^ "Leaf"
      ^ " "
      ^ string_of_int (String.length s)
  | hp, Message (h, st) ->
      headers_count_to_string hp ^ "Message (" ^ struct_to_string (h, st) ^ ")"
  | hp, Multipart parts ->
      let print_part = function
        | _h, None -> "None"
        | h, Some m -> "(" ^ struct_to_string (h, m) ^ ")"
      in
      headers_count_to_string hp
      ^ "Multi ["
      ^ String.concat "; " (List.map print_part parts)
      ^ "]"

let print_struct (h, m) = struct_to_string (h, m) |> Format.printf "%s@."

let date_to_string date =
  let open Mrmime.Date in
  match date with
  | { day; date = d, month, year; time = hour, min, sec; zone } ->
      let day =
        match day with None -> "" | Some d -> Day.to_string d ^ ", "
      in
      let sec =
        match sec with None -> "" | Some sec -> ":" ^ string_of_int sec
      in
      day
      ^ string_of_int d
      ^ " "
      ^ Month.to_string month
      ^ " "
      ^ string_of_int year
      ^ " "
      ^ string_of_int hour
      ^ ":"
      ^ string_of_int min
      ^ sec
      ^ " "
      ^ Zone.to_string zone
      ^ "\n"

let local_to_string = Mrmime.Mailbox.Local.to_string
let headers_to_string h = Header.to_stream h |> stream_to_string

let mail_to_string (m : Header.t * string Mail.t) =
  mail_to_mt m |> Mt.to_stream |> buffer_stream_to_string

let print_mail m = mail_to_string m |> Format.printf "%s@."

let print_content_encoding mail =
  Format.printf "Content-encoding: %s\n"
    Mrmime.(
      Header.content_encoding (fst mail) |> function
      | `Bit7 -> "Bit 7"
      | `Bit8 -> "Bit 8"
      | `Binary -> "Binary"
      | `Quoted_printable -> "Quoted printable"
      | `Base64 -> "Base 64"
      | `Ietf_token _ -> "Ietf_token"
      | `X_token _ -> "X_token")

let field_to_string : type a. a Field.t -> string = function
  | Mrmime.Field.Date -> "date"
  | Mailbox -> "mailbox"
  | Mailboxes -> "Mailboxes"
  | Addresses -> "addresses"
  | MessageID -> "messageId"
  | Unstructured -> "unstructured"
  | Content -> "content"
  | Encoding -> "encoding"
