let comma =
  let open Mrmime in
  Fe.using (fun () -> ',') Fe.char, ()

external identity : 'a -> 'a = "%identity"

let rec value t x =
  let open Mrmime in

  let binding t (k, v) = Fe.keval t identity Fe.[ char $ '"' ; !!string ; char $ '"' ; char $ ':' ; !!value ] k v in
  let arr = Fe.list ~sep:comma value in
  let obj = Fe.list ~sep:comma binding in

  match x with
  | `Bool true -> Fe.string t "true"
  | `Bool false -> Fe.string t "false"
  | `Null -> Fe.string t "null"
  | `Float f -> Fe.string t (Fmt.strf "%.16g" f)
  | `String s -> Fe.keval t identity Fe.[ char $ '"' ; !!string ; char $ '"' ] s
  | `A a -> Fe.keval t identity Fe.[ char $ '[' ; !!arr ; char $ ']' ] a
  | `O o -> Fe.keval t identity Fe.[ char $ '{' ; !!obj ; char $ '}' ] o

type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]
type value = [ `Null | `Bool of bool | `String of string | `Float of float ]

let json_of_input refiller input =
  let decoder = Jsonm.decoder input in

  let error (`Error err) = Fmt.invalid_arg "%a" Jsonm.pp_error err in
  let end_of_input `End = Fmt.invalid_arg "Unexpected end of input" in

  let rec arr acc k = match Jsonm.decode decoder with
    | #await -> refiller () ; arr acc k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> base (fun v -> arr (v :: acc) k) v

  and name n k = match Jsonm.decode decoder with
    | #await -> refiller () ; name n k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme v -> base (fun v -> k (n, v)) v

  and obj acc k = match Jsonm.decode decoder with
    | #await -> refiller () ; obj acc k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v -> Fmt.invalid_arg "Unexpected lexeme: %a" Jsonm.pp_lexeme v

  and base k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> Fmt.invalid_arg "Unexpected end of array/object"
    | `Name v -> Fmt.invalid_arg "Unexpected key: %s" v in

  let rec go k = match Jsonm.decode decoder with
    | #await -> refiller () ; go k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme (#Jsonm.lexeme as lexeme) -> base k lexeme in

  go identity

let json_to_output flusher output json =
  let encoder = Jsonm.encoder output in

  let flat_json json : Jsonm.lexeme list =
    let rec arr acc k = function
      | [] -> k (List.rev (`Ae :: acc))
      | (#value as x) :: r -> arr (x :: acc) k r
      | `A l :: r -> arr [ `As ] (fun l -> arr (List.rev_append l acc) k r) l
      | `O l :: r -> obj [ `Os ] (fun l -> arr (List.rev_append l acc) k r) l

    and obj acc k = function
      | [] -> k (List.rev (`Oe :: acc))
      | (n, x) :: r -> base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x

    and base k = function
      | `A l -> arr [ `As ] k l
      | `O l -> obj [ `Os ] k l
      | #value as x -> k [ x ] in

    base (fun l -> l) json in

  let rec write k = function
    | `Ok -> k ()
    | `Partial ->
      flusher (Jsonm.Manual.dst_rem encoder) ;
      write k (Jsonm.encode encoder `Await) in

  let rec go k = function
    | [] -> write k (Jsonm.encode encoder `End)
    | lexeme :: r ->
      write (fun () -> go k r) (Jsonm.encode encoder (`Lexeme lexeme)) in

  let lexemes = flat_json json in

  go identity lexemes

let json_of_string x = json_of_input (fun () -> assert false) (`String x)

let json_to_string x =
  let buf = Buffer.create 0x100 in
  json_to_output (fun _ -> assert false) (`Buffer buf) x ; Buffer.contents buf

let tests =
  [ `A [ `Bool true ]
  ; `O [ "a", `A [ `Bool true ; `Bool false ] ]
  ; `A [ `O [ "a", `Bool true ; "b", `Bool false ] ] ]

let writer_of_buf buf = fun lst ->
  let open Mrmime in

  let write a = function
    | { Encoder.IOVec.buffer= Encoder.Buffer.String x; off; len; } ->
      Buffer.add_substring buf x off len; a + len
    | { Encoder.IOVec.buffer= Encoder.Buffer.Bytes x; off; len; } ->
      Buffer.add_subbytes buf x off len; a + len
    | { Encoder.IOVec.buffer= Encoder.Buffer.Bigstring x; off; len; } ->
      Buffer.add_string buf (Bigstringaf.substring x ~off ~len); a + len in
  List.fold_left write 0 (List.rev lst)

let json =
  Alcotest.testable
    (Fmt.using json_to_string Fmt.string)
    (=)

let make v =
  let open Mrmime in

  Alcotest.test_case
    (json_to_string v)
    `Quick @@ fun () ->
  let encoder = Encoder.create 0x100 in
  let buffer = Buffer.create 0x100 in
  let t = Fe.with_writer encoder (writer_of_buf buffer) in

  let _ = Fe.eval t Fe.[ !!value ; yield ] v in
  let res = Buffer.contents buffer in

  Fmt.epr "> %s.\n%!" res ;

  let res = json_of_string res in

  Alcotest.(check json) "encode:decode:compare" v res

let () =
  Alcotest.run "fe" [ "json", List.map make tests ]
