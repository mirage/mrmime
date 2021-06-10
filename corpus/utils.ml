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

let rec mail_to_mt (mail : string Mail.t) : Mt.t =
  match mail with
  | Leaf { header; body } -> Mt.part (stream_of_string body) |> Mt.make header Mt.simple
  | Message { header; body = mes } ->
      mail_to_mt mes |> Mt.to_stream |> Mt.part |> Mt.make header Mt.simple
  | Multipart { header; body = parts } ->
      let parts : Mt.part list =
        List.map
          (function
            | Some part -> mail_to_mt part |> Mt.to_stream |> Mt.part
            | None -> Mt.part empty_stream)
          parts
      in
      Mt.multipart ~rng:Mt.rng parts |> Mt.make header Mt.multi
