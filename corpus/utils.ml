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

let build_oc dst =
  match dst with
  | `Standard -> (stdout, ignore)
  | `Filename filename ->
      let oc = open_out (Fpath.to_string filename) in
      (oc, close_out)

let print dst str =
  let oc, oc_close = build_oc dst in
  output_string oc str;
  oc_close oc

open Mrmime

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

let stream_of_string str : Mt.buffer Mt.stream =
  let consumed = ref false in
  fun () ->
    match !consumed with
    | true -> None
    | false ->
        consumed := true;
        Some (str, 0, String.length str)

let empty_stream () = None

let rec mail_to_mt (mail : Header.t * string Mail.t) : Mt.t =
  match mail with
  | header, Leaf body ->
      Mt.part (stream_of_string body) |> Mt.make header Mt.simple
  | header, Message (h, b) ->
      mail_to_mt (h, b)
      |> Mt.to_stream
      |> Mt.part ~header
      |> Mt.make Header.empty Mt.simple
  | header, Multipart parts ->
      let parts : Mt.part list =
        List.map
          (fun (h, m) ->
            match m with
            | Some m -> mail_to_mt (h, m) |> Mt.to_stream |> Mt.part
            | None -> Mt.part ~header:h empty_stream)
          parts
      in
      Mt.multipart ~header ~rng:Mt.rng parts |> Mt.make Header.empty Mt.multi

let count_header h =
  let lines =
    Header.to_stream h |> stream_to_string |> String.split_on_char '\n'
  in
  List.length lines - 1

let headers_count_to_string ~verbose h =
  if verbose then (count_header h |> string_of_int) ^ ", " else ""

let rec struct_to_string ?(verbose = false) = function
  | hp, Mail.Leaf s ->
      headers_count_to_string ~verbose hp
      ^ "Leaf"
      ^ if not verbose then "" else " " ^ string_of_int (String.length s)
  | hp, Message (h, st) ->
      headers_count_to_string ~verbose hp
      ^ "Message ("
      ^ struct_to_string ~verbose (h, st)
      ^ ")"
  | hp, Multipart parts ->
      let print_part = function
        | h, None ->
            if not verbose then "None"
            else "(" ^ headers_count_to_string ~verbose h ^ "None)"
        | h, Some m -> struct_to_string ~verbose (h, m)
      in
      headers_count_to_string ~verbose hp
      ^ "Multi ["
      ^ String.concat "; " (List.map print_part parts)
      ^ "]"

let print_struct ?(verbose = false) (h, m) =
  struct_to_string ~verbose (h, m) |> Format.printf "%s@."

let headers_to_string h = Header.to_stream h |> stream_to_string

let _print_mail (h, m) =
  let rec go h = function
    | Mail.Leaf b -> "\nLEAF\n\n" ^ headers_to_string h ^ b
    | Message (h', b) -> "\nMESSAGE\n\n" ^ headers_to_string h ^ go h' b
    | Multipart parts ->
        let parts =
          List.fold_left
            (fun acc (h, m) ->
              match m with
              | None -> ("PART\n" ^ headers_to_string h ^ "None") :: acc
              | Some m -> ("PART\n" ^ go h m) :: acc)
            [] parts
        in
        let parts = List.rev parts |> String.concat "\r\n" in
        "\nMULTIPART\n\n" ^ headers_to_string h ^ parts
  in
  go h m |> Format.printf "%s@."

let print_mail (m : Header.t * string Mail.t) =
  mail_to_mt m
  |> Mt.to_stream
  |> buffer_stream_to_string
  |> Format.printf "%s@."
