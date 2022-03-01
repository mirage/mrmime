open Mrmime

let romain_calascibetta =
  let open Mailbox in
  Local.[ w "romain"; w "calascibetta" ]
  @ Domain.(domain, [ a "gmail"; a "com" ])

let date =
  let now = Option.get (Ptime.of_float_s 1619454050.0) in
  Date.of_ptime ~zone:Date.Zone.GMT now

let subject =
  let open Unstructured.Craft in
  compile [ v "A"; sp 1; v "Simple"; sp 1; v "Email" ]

let content_disposition = Field_name.v "Content-Disposition"

let content_type_1 =
  Content_type.(make `Text (Subtype.v `Text "plain") Parameters.empty)

let content_type_2 =
  Content_type.(make `Image (Subtype.v `Image "png") Parameters.empty)

let stream_of_file filename : Mt.buffer Mt.stream =
  let tp = Bytes.create 0x1000 in
  let ic = open_in filename in
  fun () ->
    match input ic tp 0 0x1000 with
    | 0 ->
        close_in ic;
        None
    | len -> Some (Bytes.unsafe_to_string tp, 0, len)

let stream_of_string str : Mt.buffer Mt.stream =
  let consumed = ref false in
  fun () ->
    match !consumed with
    | true -> None
    | false ->
        consumed := true;
        Some (str, 0, String.length str)

let part0 =
  let header =
    let open Header in
    empty
    |> add Field_name.content_type Field.(Content, content_type_1)
    |> add Field_name.content_encoding Field.(Encoding, `Quoted_printable)
  in
  Mt.part ~header (stream_of_string "Hello World!")

let part1 =
  let header =
    let open Header in
    empty
    |> add Field_name.content_type Field.(Content, content_type_2)
    |> add Field_name.content_encoding Field.(Encoding, `Base64)
    |> add content_disposition
         Field.
           ( Unstructured,
             Unstructured.Craft.(
               compile
                 [ v "attachement";
                   sp 0;
                   v ";";
                   sp 1;
                   v "filename";
                   sp 0;
                   v "=";
                   sp 0;
                   v "mrmime.png"
                 ]) )
  in
  Mt.part ~header (stream_of_file "mrmime.png")

let header =
  let open Header in
  empty
  |> add Field_name.date Field.(Date, date)
  |> add Field_name.subject Field.(Unstructured, subject)
  |> add Field_name.from Field.(Mailboxes, [ romain_calascibetta ])
  |> add (Field_name.v "To")
       Field.(Addresses, Address.[ mailbox romain_calascibetta ])

let rng ?g:_ len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    match Random.int (26 + 26 + 10) with
    | n when n < 26 -> Bytes.set res i (Char.chr (Char.code 'a' + n))
    | n when n < 52 -> Bytes.set res i (Char.chr (Char.code 'A' + (n - 26)))
    | n -> Bytes.set res i (Char.chr (Char.code '0' + (n - 26 - 26)))
  done;
  Bytes.unsafe_to_string res

let boundary = "foo"

let email =
  Mt.make header Mt.multi (Mt.multipart ~rng:Mt.rng ~boundary [ part0; part1 ])

let email =
  let stream = Mt.to_stream email in
  let buffer = Buffer.create 0x1000 in
  let rec go () =
    match stream () with
    | Some (str, off, len) ->
        Buffer.add_substring buffer str off len;
        go ()
    | None -> Buffer.contents buffer
  in
  go ()

let () = print_string email
