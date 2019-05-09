type t = Rfc5322.unstructured

let pp_atom ppf = function
  | `Text x -> Fmt.quote Fmt.string ppf x
  | `WSP wsp -> Fmt.pf ppf "<WSP:%s>"wsp
  | `CR n -> Fmt.pf ppf "<CR:%d>" n
  | `LF n -> Fmt.pf ppf "<LF:%d>" n
  | `CRLF -> Fmt.pf ppf "<CRLF>@\n"
  | `Encoded t -> Encoded_word.pp ppf t

let pp : t Fmt.t = Fmt.list ~sep:(Fmt.always "@,") pp_atom

let equal_atom a b = match a, b with
  | `Text a, `Text b -> String.equal a b
  | `WSP a, `WSP b -> String.equal a b
  | `CR a, `CR b -> a = b
  | `LF a, `LF b -> a = b
  | `CRLF, `CRLF -> true
  | `Encoded a, `Encoded b -> Encoded_word.equal a b
  | _, _ -> false

let equal a b =
  try List.for_all2 equal_atom a b
  with _ -> false

module Encoder = struct
  include Encoder

  let element ppf = function
    | `Text x -> string ppf x
    | `WSP x -> string ppf x
    | `CR n -> string ppf (String.make n '\r')
    | `LF n -> string ppf (String.make n '\n')
    | `CRLF -> string ppf "\r\n"
    | `Encoded x -> Encoded_word.Encoder.encoded_word ppf x

  let cut = (fun ppf () -> eval ppf [ cut ]), ()
  let unstructured : Rfc5322.unstructured t = list ~sep:cut element
end

let to_unstructured ~field_name gen value =
  let buf = Buffer.create 0x100 in
  let emitter =
    let write a x =
      let open Encoder.IOVec in
      let open Encoder.Buffer in
      match x with
      | { buffer= String x; off; len; } ->
        Buffer.add_substring buf x off len ; a + len
      | { buffer= Bytes x; off; len; } ->
        Buffer.add_subbytes buf x off len ; a + len
      | { buffer= Bigstring x; off; len; } ->
        let x = Bigstringaf.substring x ~off ~len in
        Buffer.add_string buf x ; a + len in
    List.fold_left write 0 in
  let encoder = Encoder.create ~margin:78 ~new_line:"\r\n" ~emitter 0x100 in
  let () =
    let open Encoder in
    keval (fun encoder -> assert (Encoder.is_empty encoder) ; ()) encoder
      [ !!Field_name.Encoder.field_name; char $ ':'
      ; spaces 1; bbox; !!gen; close; new_line ] field_name value in
  let res = Buffer.contents buf in
  let parser =
    let open Angstrom in
    let open Rfc5322 in
    Rfc5322.field_name
    <* many (satisfy (function '\x09' .. '\x20' -> true | _ -> false))
    <* char ':'
    >>= fun field_name -> unstructured
    >>= fun value -> return (Field_name.v field_name, value) in
  match Angstrom.parse_string parser res with
  | Ok (field, unstructured) ->
    assert (Field_name.equal field field_name) ;
    unstructured
  | Error _ ->
    Fmt.failwith "Normalized value %S can not be considered as a unstructured value."
      res
