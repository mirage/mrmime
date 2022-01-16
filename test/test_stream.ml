open Mrmime

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let blit src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let parse ~emitters =
  let parser = Mail.stream emitters in
  let state = ref (Angstrom.Unbuffered.parse parser) in
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  fun data ->
    match !state with
    | Angstrom.Unbuffered.Done (_, tree) -> `Done tree
    | Fail _ -> `Fail
    | Partial { committed; continue } ->
        Ke.Rke.N.shift_exn ke committed;
        if committed = 0 then Ke.Rke.compress ke;
        let () =
          match data with
          | `String "" -> ()
          | `String str ->
              Ke.Rke.N.push ke ~blit ~length:String.length ~off:0
                ~len:(String.length str) str;
              let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
              state :=
                continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete
          | `Eof -> (
              match Ke.Rke.N.peek ke with
              | [] -> state := continue Bigstringaf.empty ~off:0 ~len:0 Complete
              | [ slice ] ->
                  state :=
                    continue slice ~off:0 ~len:(Bigstringaf.length slice)
                      Complete
              | slice :: _ ->
                  state :=
                    continue slice ~off:0 ~len:(Bigstringaf.length slice)
                      Incomplete)
        in
        `Continue

let rec map f = function
  | Mail.Leaf body -> Mail.Leaf (f body)
  | Multipart parts ->
      Multipart
        (List.map
           (function
             | header, None -> (header, None)
             | header, Some body -> (header, Some (map f body)))
           parts)
  | Message (header, body) -> Message (header, map f body)

open Lwt.Infix

let stream ?(bounds = 10) ~identify stream =
  let output, push = Lwt_stream.create () in
  let q = Queue.create () in
  let fresh_id =
    let r = ref 0 in
    fun () ->
      incr r;
      !r
  in
  let tbl = Hashtbl.create 0x10 in
  let emitters header =
    let id = fresh_id () in
    Queue.push (`Id (header, id)) q;
    ((fun data -> Queue.push (`Data (id, data)) q), id)
  in
  let parse = parse ~emitters in
  let rec go () =
    match Queue.pop q with
    | `Id (header, id) ->
        let client_id = identify header in
        let stream, bounded_emitter = Lwt_stream.create_bounded bounds in
        Hashtbl.add tbl id (client_id, stream, bounded_emitter);
        push (Some (client_id, header, stream));
        go ()
    | `Data (id, Some data) ->
        let _, _, emitter = Hashtbl.find tbl id in
        emitter#push data >>= fun () -> go ()
    | `Data (id, None) ->
        let _, _, emitter = Hashtbl.find tbl id in
        emitter#close;
        go ()
    | exception Queue.Empty -> (
        Lwt_stream.get stream >>= fun data ->
        let data = match data with Some s -> `String s | None -> `Eof in
        match parse data with
        | `Continue -> go ()
        | `Done (header, t) ->
            let client_id_of_id id =
              let client_id, _, _ = Hashtbl.find tbl id in
              client_id
            in
            push None;
            Lwt.return_ok (header, map client_id_of_id t)
        | `Fail ->
            push None;
            Lwt.return_error (`Msg "Invalid email"))
  in
  (`Parse (go ()), output)

let blit src src_off dst dst_off len =
  Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

let line_of_queue queue =
  let exists ~p queue =
    let pos = ref 0 and res = ref (-1) in
    Ke.Rke.iter
      (fun chr ->
        if p chr && !res = -1 then res := !pos;
        incr pos)
      queue;
    if !res = -1 then None else Some !res
  in
  match exists ~p:(( = ) '\n') queue with
  | None -> None
  | Some 0 ->
      Ke.Rke.N.shift_exn queue 1;
      Some ""
  | Some pos ->
      let tmp = Bytes.create pos in
      Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp;
      Ke.Rke.N.shift_exn queue (pos + 1);
      Some (Bytes.to_string tmp)

let blit src src_off dst dst_off len =
  Bigstringaf.blit_from_bytes src ~src_off dst ~dst_off ~len

let rec getline fd tmp queue =
  match line_of_queue queue with
  | Some line -> Lwt.return_some line
  | None -> (
      Lwt_unix.read fd tmp 0 (Bytes.length tmp) >>= function
      | 0 -> Lwt.return_none
      | len ->
          Ke.Rke.N.push queue ~blit ~length:Bytes.length ~off:0 ~len tmp;
          getline fd tmp queue)

let getline fd =
  let tmp = Bytes.create 0x1000 in
  let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  fun () -> getline fd tmp queue

let rec copy filename stream =
  Lwt_unix.openfile filename Unix.[ O_RDWR; O_CREAT; O_TRUNC ] 0o644
  >>= fun fd ->
  transmit fd stream >>= fun () -> Lwt_unix.close fd

and transmit fd stream =
  Lwt_stream.get stream >>= function
  | Some str ->
      fully_write fd str 0 (String.length str) >>= fun () -> transmit fd stream
  | None -> Lwt.return_unit

and fully_write fd str off len =
  if len <= 0 then Lwt.return_unit
  else
    Lwt_unix.write_string fd str off len >>= fun len' ->
    fully_write fd str (off + len') (len - len')

let rec consume stream =
  Lwt_stream.get stream >>= function
  | Some (uid, _header, body) ->
      copy (Fmt.str "stdout-%d" uid) body >>= fun () -> consume stream
  | None -> Lwt.return_unit

let ( <.> ) f g x = f (g x)

let explode () =
  let identify =
    let v = ref (-1) in
    fun _ ->
      incr v;
      !v
  in
  let stdin =
    Lwt_stream.from
      (Lwt.map (Option.map (fun str -> str ^ "\n")) <.> getline Lwt_unix.stdin)
  in
  let `Parse th, stream = stream ~identify stdin in
  Lwt.both th (consume stream) >>= fun (v, ()) ->
  match v with
  | Ok _ -> Lwt.return_unit
  | Error _ -> invalid_arg "Invalid email"

let () = Lwt_main.run (explode ())
