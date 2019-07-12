type 'x stream = (unit -> 'x option)
type buffer = string * int * int

type field = Field_name.t * Unstructured.t

let iter ~f buf ~off ~len = for i = off to len - 1 do f buf.[i] done

let to_quoted_printable : ?length:int -> buffer stream -> buffer stream = fun ?length:(chunk_length= 4096) stream ->
  let chunk_length = 4096 in
  let chunk = Bytes.create chunk_length in
  let encoder = Pecu.encoder `Manual in
  let queue = Ke.Rke.create ~capacity:128 Bigarray.Int in

  let rec emit () =
    Ke.Rke.cons queue 256 ;
    let len = chunk_length - Pecu.dst_rem encoder in
    Some (Bytes.unsafe_to_string chunk, 0, len)

  and pending = function
    | `Ok -> go ()
    | `Partial ->
      let len = chunk_length - Pecu.dst_rem encoder in
      Some (Bytes.unsafe_to_string chunk, 0, len)

  and go () = match Ke.Rke.pop_exn queue with
    | 256 (* Await *) ->
      Pecu.dst encoder chunk 0 chunk_length ;
      ( match Pecu.encode encoder `Await with
        | `Ok -> go ()
        | `Partial -> emit () )
    | 257 (* End *) ->
      Ke.Rke.cons queue 258 ;
      pending @@ Pecu.encode encoder `End
    | 258 -> assert (Pecu.encode encoder `Await = `Ok) ; Ke.Rke.cons queue 258 ; None
    | chr ->
      ( match Pecu.encode encoder (`Char (Char.chr chr)) with
        | `Ok -> go ()
        | `Partial -> emit () )
    | exception Ke.Rke.Empty ->
      match stream () with
      | Some (buf, off, len) -> iter ~f:(fun chr -> Ke.Rke.push queue (Char.code chr)) buf ~off ~len ; go ()
      | None -> Ke.Rke.push queue 257 ; go () in

  Pecu.dst encoder chunk 0 chunk_length ; go

let to_base64 : ?length:int -> buffer stream -> buffer stream = fun ?length:(chunk_length= 4096) stream ->
  let chunk = Bytes.create chunk_length in
  let encoder = Base64_rfc2045.encoder `Manual in
  let queue = Ke.Rke.create ~capacity:128 Bigarray.Int in

  let rec emit () =
    Ke.Rke.cons queue 256 ;
    let len = chunk_length - Base64_rfc2045.dst_rem encoder in
    Some (Bytes.unsafe_to_string chunk, 0, len)

  and pending = function
    | `Ok -> go ()
    | `Partial ->
      let len = chunk_length - Base64_rfc2045.dst_rem encoder in
      Some (Bytes.unsafe_to_string chunk, 0, len)

  and go () = match Ke.Rke.pop_exn queue with
    | 256 (* Await *) ->
      Base64_rfc2045.dst encoder chunk 0 chunk_length ;
      ( match Base64_rfc2045.encode encoder `Await with
        | `Ok -> go ()
        | `Partial -> emit () )
    | 257 (* End *) ->
      Ke.Rke.cons queue 258 ;
      pending @@ Base64_rfc2045.encode encoder `End
    | 258 -> assert (Base64_rfc2045.encode encoder `Await = `Ok) ; Ke.Rke.cons queue 258 ; None
    | chr ->
      ( match Base64_rfc2045.encode encoder (`Char (Char.chr chr)) with
        | `Ok -> go ()
        | `Partial -> emit () )
    | exception Ke.Rke.Empty ->
      match stream () with
      | Some (buf, off, len) -> iter ~f:(fun chr -> Ke.Rke.push queue (Char.code chr)) buf ~off ~len ; go ()
      | None -> Ke.Rke.push queue 257 ; go () in

  Base64_rfc2045.dst encoder chunk 0 chunk_length ; go

type part =
  { content : Content.t
  ; fields : field list
  ; body : buffer stream }

type multipart =
  { content : Content.t
  ; fields : field list
  ; parts : part list }

let part ?(content= Content.default) ?(fields= []) stream =
  if not (Content.is_discrete content)
  then Fmt.invalid_arg "Content-type MUST be discrete type to make a part" ;
  let stream = match Content.encoding content with
    | `Quoted_printable -> to_quoted_printable stream
    | `Base64 -> to_base64 stream
    | `Bit8 | `Binary | `Bit7 -> stream
    | `Ietf_token _ | `X_token _ -> assert false in (* XXX(dinosaure): TODO [`Bit7], IETF and extension encoding. *)
  { content; fields; body= stream; }

let multipart_content_default =
  let open Content_type in
  Content.make (make `Multipart (Subtype.v `Multipart "mixed") Parameters.empty)

type 'g rng = ?g:'g -> int -> string
external random_seed : unit -> int array = "caml_sys_random_seed"

let rng ?(g= random_seed ()) n =
  Random.full_init g ;
  let res = Bytes.create n in
  for i = 0 to n - 1 do Bytes.set res i (Random.int 256 |> Char.chr) done ;
  Bytes.unsafe_to_string res

let multipart ~rng ?(content= multipart_content_default) ?boundary ?(fields= []) parts =
  if not (Content.is_multipart content)
  then Fmt.invalid_arg "Content-type MUST be multipart" ;
  let content = match Content.boundary content, boundary with
    | Some _, None -> content
    | (None | Some _), Some boundary ->
      let boundary = Content_type.Parameters.value_exn boundary in
      Content.add_parameter ~key:"boundary" ~value:boundary content
    | None, None ->
      let boundary =
        let raw = rng ?g:None 8 in
        let pp = Fmt.iter ~sep:(fun _ _ -> ()) String.iter (fun ppf chr -> Fmt.pf ppf "%02x" (Char.code chr)) in
        Fmt.strf "%a" pp raw in
      Content.add_parameter ~key:"boundary" ~value:(Content_type.Parameters.value_exn boundary) content in
  { content; fields; parts; }

let none = (fun () -> None)

let concat s0 s1 =
  let c = ref s0 in
  let rec go () = match !c () with
    | Some x -> Some x
    | None -> if !c == s0 then ( c := s1 ; go ()) else None in
  go

let stream_of_string x =
  let once = ref false in
  let go () = if !once then None else ( once := true ; Some (x, 0, String.length x)) in go

let crlf () = stream_of_string "\r\n"

let ( @ ) a b = concat a b

let map f stream =
  let go () = match stream () with
    | Some v -> Some (f v)
    | None -> None in
  go

let stream_of_part { content; fields= _; body; } =
  let content_stream = map (fun s -> s, 0, String.length s) (Encoder.to_stream Content.Encoder.content content) in
  content_stream @ crlf () @ body

(* XXX(dinosaure): hard part to compile multiple parts under one. *)
let multipart_as_part : multipart -> part = fun { content; fields; parts; } ->
  let boundary = match Content.boundary content with
    | Some (`String v) | Some (`Token v) -> v
    | None -> Fmt.failwith "Multipart MUST have a boundary" (* XXX(dinosaure): should never occur! *)in
  let beginner = Rfc2046.make_dash_boundary boundary ^ "\r\n" in
  let inner = Rfc2046.make_delimiter boundary ^ "\r\n" in
  let closer = Rfc2046.make_close_delimiter boundary ^ "\r\n" in

  let rec go stream = function
    | [] -> none
    | [ x ] -> stream @ (stream_of_part x) @ (stream_of_string closer)
    | x :: r ->
      let stream = stream @ (stream_of_part x) @ (stream_of_string inner) in
      go stream r in

  { content; fields; body= go (stream_of_string beginner) parts }

type 'x body = Simple : part body | Multi : multipart body

let simple = Simple
let multi = Multi

type t =
  { header : Header.t
  ; body : buffer stream }

let rec make
  : type a. Header.t -> a body -> a -> t
  = fun header kind v -> match kind with
    | Simple ->
      let merge n o = match n, o with
        | Some n, Some o -> Some n
        | Some x, None | None, Some x -> Some x
        | None, None -> None in
      let { content= c; fields; body; } = v in
      let c = Content.merge merge c (Header.content header) in
      let header = Header.add_or_replace Field.(Content $ c) header in
      let header = List.fold_left (fun t (f, v) -> Header.add Field.(Field f $ v) t) header fields in
      { header; body; }
    | Multi ->
      let part = multipart_as_part v in
      make header Simple part

let to_stream t : buffer stream =
  let header_stream = Header.to_stream t.header in
  let body_stream = t.body in
  (map (fun s -> s, 0, String.length s) header_stream) @ crlf () @ body_stream
