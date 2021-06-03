let parsers =
  let open Mrmime in
  let unstructured = Field.(Witness Unstructured) in
  let open Field_name in
  Map.empty
  |> Map.add date unstructured
  |> Map.add from unstructured
  |> Map.add sender unstructured
  |> Map.add reply_to unstructured
  |> Map.add (v "To") unstructured
  |> Map.add cc unstructured
  |> Map.add bcc unstructured
  |> Map.add subject unstructured
  |> Map.add message_id unstructured
  |> Map.add comments unstructured
  |> Map.add content_type unstructured
  |> Map.add content_encoding unstructured

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
    match v () with
    | Some str ->
        go (str :: acc)()
    | None -> acc
  in
  String.concat "" (List.rev (go [] ()))


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
