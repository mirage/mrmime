type 'x stream = unit -> 'x option
type buffer = string * int * int
type field = Field_name.t * Unstructured.t

let iter ~f buf ~off ~len =
  for i = off to len - 1 do
    f buf.[i]
  done

let to_quoted_printable : ?length:int -> buffer stream -> buffer stream =
 fun ?length:(chunk_length = 4096) stream ->
  let chunk = Bytes.create chunk_length in
  let encoder = Pecu.encoder `Manual in
  let queue = Ke.Rke.create ~capacity:128 Bigarray.Int in

  let rec emit () =
    Ke.Rke.cons queue 256;
    let len = chunk_length - Pecu.dst_rem encoder in
    Some (Bytes.unsafe_to_string chunk, 0, len)
  and pending = function
    | `Ok -> go ()
    | `Partial ->
        let len = chunk_length - Pecu.dst_rem encoder in
        Some (Bytes.unsafe_to_string chunk, 0, len)
  and go () =
    match Ke.Rke.pop_exn queue with
    | 256 (* Await *) -> (
        Pecu.dst encoder chunk 0 chunk_length;
        match Pecu.encode encoder `Await with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    (* XXX(dinosaure): [257] was an old state which is not used anymore. *)
    | 258 (* End *) ->
        Ke.Rke.cons queue 259;
        (pending [@tailcall]) (Pecu.encode encoder `End)
    | 259 ->
        assert (Pecu.encode encoder `Await = `Ok);
        Ke.Rke.cons queue 259;
        None
    | 260 -> (
        match Pecu.encode encoder `Line_break with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    | chr -> (
        match Pecu.encode encoder (`Char (Char.chr chr)) with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    | exception Ke.Rke.Empty -> (
        match stream () with
        | Some (buf, off, len) ->
            iter ~f:(fun chr -> Ke.Rke.push queue (Char.code chr)) buf ~off ~len;
            Ke.Rke.push queue 260;
            (go [@tailcall]) ()
        | None ->
            Ke.Rke.push queue 258;
            (go [@tailcall]) ())
  in

  Pecu.dst encoder chunk 0 chunk_length;
  go

let to_base64 : ?length:int -> buffer stream -> buffer stream =
 fun ?length:(chunk_length = 4096) stream ->
  let chunk = Bytes.create chunk_length in
  let encoder = Base64_rfc2045.encoder `Manual in
  let queue = Ke.Rke.create ~capacity:128 Bigarray.Int in

  let rec emit () =
    Ke.Rke.cons queue 256;
    let len = chunk_length - Base64_rfc2045.dst_rem encoder in
    Some (Bytes.unsafe_to_string chunk, 0, len)
  and pending = function
    | `Ok -> (go [@tailcall]) ()
    | `Partial ->
        let len = chunk_length - Base64_rfc2045.dst_rem encoder in
        Some (Bytes.unsafe_to_string chunk, 0, len)
  and go () =
    match Ke.Rke.pop_exn queue with
    | 256 (* Await *) -> (
        Base64_rfc2045.dst encoder chunk 0 chunk_length;
        match Base64_rfc2045.encode encoder `Await with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    | 257 (* End *) ->
        Ke.Rke.cons queue 258;
        (pending [@tailcall]) (Base64_rfc2045.encode encoder `End)
    | 258 ->
        assert (Base64_rfc2045.encode encoder `Await = `Ok);
        Ke.Rke.cons queue 258;
        None
    | chr -> (
        match Base64_rfc2045.encode encoder (`Char (Char.chr chr)) with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    | exception Ke.Rke.Empty -> (
        match stream () with
        | Some (buf, off, len) ->
            iter ~f:(fun chr -> Ke.Rke.push queue (Char.code chr)) buf ~off ~len;
            (go [@tailcall]) ()
        | None ->
            Ke.Rke.push queue 257;
            (go [@tailcall]) ())
  in

  Base64_rfc2045.dst encoder chunk 0 chunk_length;
  go

type part = { header : Header.t; body : buffer stream }
type multipart = { header : Header.t; parts : part list }

let part ?(encoding = true) ?(header = Header.empty) stream =
  let content_encoding = Header.content_encoding header in
  let stream =
    if encoding then
      match content_encoding with
      | `Quoted_printable -> to_quoted_printable stream
      | `Base64 -> to_base64 stream
      | `Bit8 | `Binary | `Bit7 -> stream
      | `Ietf_token _ | `X_token _ -> assert false
    else stream
  in
  (* XXX(dinosaure): TODO [`Bit7], IETF and extension encoding. *)
  { header; body = stream }

let multipart_content_default =
  let open Content_type in
  Content_type.make `Multipart (Subtype.v `Multipart "mixed") Parameters.empty

type 'g rng = ?g:'g -> int -> string

external random_seed : unit -> int array = "caml_sys_random_seed"

let rng ?(g = random_seed ()) n =
  Random.full_init g;
  let res = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set res i (Random.int 256 |> Char.chr)
  done;
  Bytes.unsafe_to_string res |> Base64.encode_exn

let multipart ?g ~rng ?(header = Header.empty) ?boundary parts =
  let boundary =
    match boundary with Some boundary -> boundary | None -> rng ?g 8
  in
  let boundary = Content_type.Parameters.v boundary in
  let content_type =
    if Header.exists Field_name.content_type header then
      Header.content_type header
    else multipart_content_default
  in
  let content_type =
    match Content_type.boundary content_type with
    | None -> Content_type.with_parameter content_type ("boundary", boundary)
    | Some _ -> content_type
  in
  let header =
    Header.replace Field_name.content_type (Field.Content, content_type) header
  in
  { header; parts }

let concat s0 s1 =
  let c = ref s0 in
  let rec go () =
    match !c () with
    | Some x -> Some x
    | None ->
        if !c == s0 then (
          c := s1;
          go ())
        else None
  in
  go

let stream_of_string x =
  let once = ref false in
  let go () =
    if !once then None
    else (
      once := true;
      Some (x, 0, String.length x))
  in
  go

let stream_of_lines lines =
  let lines = ref lines in
  let go () =
    match !lines with
    | [] -> None
    | x :: r ->
        lines := r;
        Some (x ^ "\r\n", 0, String.length x + 2)
  in
  go

let crlf () = stream_of_string "\r\n"
let ( @ ) a b = concat a b

let map f stream =
  let go () = match stream () with Some v -> Some (f v) | None -> None in
  go

let stream_of_part { header; body } =
  let content_stream =
    map
      (fun s -> (s, 0, String.length s))
      (Prettym.to_stream Header.Encoder.header header)
  in
  content_stream @ crlf () @ body

(* XXX(dinosaure): hard part to compile multiple parts under one. *)
let multipart_as_part : multipart -> part =
 fun { header; parts } ->
  let boundary =
    match Content_type.boundary (Header.content_type header) with
    | Some v -> v
    | None -> failwith "Multipart MUST have a boundary"
    (* XXX(dinosaure): should never occur! *)
  in
  let beginner = Rfc2046.make_dash_boundary boundary ^ "\r\n" in
  (* XXX(dinosaure): we must respect one rule, emit line per line. *)
  let inner () = stream_of_lines [ ""; Rfc2046.make_dash_boundary boundary ] in
  let closer =
    stream_of_lines [ ""; Rfc2046.make_dash_boundary boundary ^ "--" ]
  in

  let rec go stream = function
    | [] -> assert false
    | [ x ] -> stream @ stream_of_part x @ closer
    | x :: r ->
        let stream = stream @ stream_of_part x @ inner () in
        go stream r
  in

  { header; body = go (stream_of_string beginner) parts }

type 'x body = Simple : part body | Multi : multipart body

let simple = Simple
let multi = Multi

type t = { header : Header.t; body : buffer stream }

let rec make : type a. Header.t -> a body -> a -> t =
 fun header kind v ->
  match kind with
  | Simple ->
      let ({ header = header'; body } : part) = v in
      { header = Header.concat header header'; body }
  | Multi ->
      let part = multipart_as_part v in
      make header Simple part

let to_stream t : buffer stream =
  let header_stream = Header.to_stream t.header in
  let body_stream = t.body in
  map (fun s -> (s, 0, String.length s)) header_stream @ crlf () @ body_stream
