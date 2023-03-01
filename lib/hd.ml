type parsers = Field.witness Field_name.Map.t
type value = [ `Field of Field.field Location.with_location | `End ]
type state = value Angstrom.Unbuffered.state

type decoder =
  { queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    parsers : parsers;
    mutable closed : bool;
    mutable state : state
  }

let field g =
  let open Angstrom in
  let is_wsp = function ' ' | '\t' -> true | _ -> false in
  Field_name.Decoder.field_name >>= fun field_name ->
  skip_while is_wsp *> char ':' *> Field.Decoder.field ~g field_name

let with_location p =
  let open Angstrom in
  pos >>= fun a ->
  p >>= fun v ->
  pos >>| fun b ->
  let location = Location.make a b in
  Location.inj ~location v

let parser parsers =
  let open Angstrom in
  let crlf = char '\r' *> char '\n' in
  with_location (field parsers) >>| (fun v -> `Field v) <|> crlf *> return `End

let decoder parsers =
  { queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char;
    parsers;
    closed = false;
    state = Angstrom.Unbuffered.parse (parser parsers)
  }

type decode =
  [ `Field of Field.field Location.with_location
  | `Await
  | `End of string
  | `Malformed of string ]

let rec decode : decoder -> decode =
 fun decoder ->
  match decoder.state with
  | Angstrom.Unbuffered.Partial { committed; continue } ->
      Ke.Rke.N.shift_exn decoder.queue committed;
      if committed = 0 then Ke.Rke.compress decoder.queue;
      let more =
        match decoder.closed with
        | true -> Angstrom.Unbuffered.Complete
        | false -> Angstrom.Unbuffered.Incomplete
      in
      let len = Ke.Rke.length decoder.queue in
      let slice =
        if len = 0 then Bigstringaf.empty
        else List.hd (Ke.Rke.N.peek decoder.queue)
      in
      if len > 0 || decoder.closed then (
        decoder.state <-
          continue slice ~off:0 ~len:(Bigstringaf.length slice) more;
        protect decoder)
      else `Await
  | Angstrom.Unbuffered.Fail (committed, _, err) ->
      Ke.Rke.N.shift_exn decoder.queue committed;
      `Malformed err
  | Angstrom.Unbuffered.Done (committed, `End) -> (
      Ke.Rke.N.shift_exn decoder.queue committed;
      Ke.Rke.compress decoder.queue;
      match Ke.Rke.N.peek decoder.queue with
      | [ x ] -> `End (Bigstringaf.to_string x)
      | [] -> `End ""
      | _ -> assert false)
  | Angstrom.Unbuffered.Done (committed, `Field v) ->
      Ke.Rke.N.shift_exn decoder.queue committed;
      decoder.state <- Angstrom.Unbuffered.parse (parser decoder.parsers);
      `Field v

and protect decoder =
  match decoder.state with
  | Angstrom.Unbuffered.Partial { committed = 0; _ } -> `Await
  | _ -> decode decoder

let blit_from_string src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let src decoder src off len =
  if off < 0 || len < 0 || off + len > String.length src then
    invalid_arg "Invalid bounds";
  Ke.Rke.N.push decoder.queue ~blit:blit_from_string ~length:String.length ~off
    ~len src;
  if len = 0 then decoder.closed <- true
