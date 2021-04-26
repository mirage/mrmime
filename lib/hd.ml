module G = Field_name.Map
module Q = Ke.Rke.Weighted

type q = (char, Bigarray.int8_unsigned_elt) Q.t
type g = Field.witness G.t
type v = [ `Field of Field.field Location.with_location | `End ]
type s = v Angstrom.Unbuffered.state

type decoder = {
  q : q;
  b : Bigstringaf.t;
  p : g;
  mutable c : bool;
  mutable s : s;
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

let parser g =
  let open Angstrom in
  let crlf = char '\r' *> char '\n' in
  with_location (field g) >>| (fun v -> `Field v) <|> crlf *> return `End

let decoder ?(p = G.empty) buffer =
  {
    q = Q.from buffer;
    b = buffer;
    p;
    c = false;
    s = Angstrom.Unbuffered.parse (parser p);
  }

type decode =
  [ `Field of Field.field Location.with_location
  | `Await
  | `End of string
  | `Malformed of string ]

let rec decode : decoder -> decode =
 fun decoder ->
  match decoder.s with
  | Angstrom.Unbuffered.Partial { committed; continue } ->
      Q.N.shift_exn decoder.q committed;
      Q.compress decoder.q;
      let more =
        if decoder.c then Angstrom.Unbuffered.Complete
        else Angstrom.Unbuffered.Incomplete
      in
      let off = 0 and len = Q.length decoder.q in
      if len > 0 || decoder.c then (
        decoder.s <- continue decoder.b ~off ~len more;
        protect decoder)
      else `Await
  | Angstrom.Unbuffered.Fail (committed, _, err) ->
      Q.N.shift_exn decoder.q committed;
      `Malformed err
  | Angstrom.Unbuffered.Done (committed, `End) -> (
      Q.N.shift_exn decoder.q committed;
      Q.compress decoder.q;
      match Q.N.peek decoder.q with
      | [ x ] -> `End (Bigstringaf.to_string x)
      | [] -> `End ""
      | _ -> assert false)
  | Angstrom.Unbuffered.Done (committed, `Field v) ->
      Q.N.shift_exn decoder.q committed;
      decoder.s <- Angstrom.Unbuffered.parse (parser decoder.p);
      `Field v

and protect decoder =
  match decoder.s with
  | Angstrom.Unbuffered.Partial { committed = 0; _ } -> `Await
  | _ -> decode decoder

let blit_from_string src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let src decoder source off len =
  if off < 0 || len < 0 || off + len > String.length source then
    Fmt.invalid_arg "Invalid bounds"
  else
    Q.N.push decoder.q ~blit:blit_from_string ~length:String.length ~off ~len
      source
    |> function
    | Some _ ->
        if len = 0 then decoder.c <- true;
        Rresult.R.ok ()
    | None -> Rresult.R.error_msg "Input is too much bigger"
