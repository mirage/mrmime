open Mrmime

let romain =
  let open Mailbox in
  Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ])

let now =
  let open Date in
  match Date.of_ptime ~zone:Zone.gmt (Ptime_clock.now ()) with
  | Ok v -> v
  | Error (`Msg err) -> failwith err

let c =
  let content_type =
    let open Content_type in
    make `Multipart (Subtype.iana_exn `Multipart "mixed")
      Parameters.(of_list [ key_exn "boundary", value_exn "my-special-boundary" ]) in
  Content.make content_type

let stream_of_ic ic =
  let tmp = Bytes.create 4096 in
  let go () = match input ic tmp 0 4096 with
    | 0 -> None
    | len ->
      Some (Bytes.unsafe_to_string tmp, 0, len)
    | exception End_of_file -> None in
  go

let stream_of_file path =
  let tmp = Bytes.create 4096 in
  let closed = ref false in
  let ic = open_in path in

  let go () = match input ic tmp 0 4096 with
    | 0 -> if not !closed then ( closed := true ; close_in ic ) ; None
    | len -> Some (Bytes.unsafe_to_string tmp, 0, len)
    | exception End_of_file ->
      if not !closed then ( closed := true ; close_in ic ) ; None in
  go

let part_0 =
  let content =
    let content_type =
      let open Content_type in
      make `Text (Subtype.iana_exn `Text "plain")
        Parameters.(of_list [ key_exn "charset", value_exn "utf-8" ]) in
    Content.make ~encoding:`Quoted_printable content_type in
  Mt.part ~content (stream_of_ic stdin)

let part_1 =
  let content =
    let content_type =
      let open Content_type in
      make `Image (Subtype.iana_exn `Image "png") Parameters.empty in
    Content.make ~encoding:`Base64 content_type in
  Mt.part ~content (stream_of_file Sys.argv.(1))

let multipart = Mt.multipart ~content:c [ part_0; part_1 ]

let header =
  let open Header in
  Field.(From $ [ romain ])
  & Field.(Date $ now)
  & Field.(To $ Address.[ mailbox romain ])
  & Field.(Sender $ romain)
  & empty

let mail = Mt.make header Mt.multi multipart
let stream = Mt.to_stream mail

let () =
  let rec go () = match stream () with
    | Some (buf, off, len) ->
      output_substring stdout buf off len ;
      go ()
    | None -> () in
  go ()
