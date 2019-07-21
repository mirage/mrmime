let comma =
  (fun t () -> Encoder.char t ','), ()

let rec value t x =
  let binding t (k, v) =
    Encoder.eval t
      Encoder.[char $ '"'; !!string; char $ '"'; char $ ':'; !!value]
      k v
  in
  let arr = Encoder.list ~sep:comma value in
  let obj = Encoder.list ~sep:comma binding in
  match x with
  | `Bool true -> Encoder.string t "true"
  | `Bool false -> Encoder.string t "false"
  | `Null -> Encoder.string t "null"
  | `Float f -> Encoder.string t (Fmt.strf "%.16g" f)
  | `String s -> Encoder.eval t Encoder.[char $ '"'; !!string; char $ '"'] s
  | `A a -> Encoder.eval t Encoder.[char $ '['; !!arr; char $ ']'] a
  | `O o -> Encoder.eval t Encoder.[char $ '{'; !!obj; char $ '}'] o

type await = [`Await]
type error = [`Error of Jsonm.error]
type eoi = [`End]
type value = [`Null | `Bool of bool | `String of string | `Float of float]

let json_of_input refiller input =
  let decoder = Jsonm.decoder input in
  let error (`Error err) = Fmt.invalid_arg "%a" Jsonm.pp_error err in
  let end_of_input `End = Fmt.invalid_arg "Unexpected end of input" in
  let rec arr acc k =
    match Jsonm.decode decoder with
    | #await -> refiller () ; arr acc k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> base (fun v -> arr (v :: acc) k) v
  and name n k =
    match Jsonm.decode decoder with
    | #await -> refiller () ; name n k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme v -> base (fun v -> k (n, v)) v
  and obj acc k =
    match Jsonm.decode decoder with
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
    | `Name v -> Fmt.invalid_arg "Unexpected key: %s" v
  in
  let rec go k =
    match Jsonm.decode decoder with
    | #await -> refiller () ; go k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme (#Jsonm.lexeme as lexeme) -> base k lexeme
  in
  go (fun x -> x)

let json_to_output flusher output json =
  let encoder = Jsonm.encoder output in
  let flat_json json : Jsonm.lexeme list =
    let rec arr acc k = function
      | [] -> k (List.rev (`Ae :: acc))
      | (#value as x) :: r -> arr (x :: acc) k r
      | `A l :: r -> arr [`As] (fun l -> arr (List.rev_append l acc) k r) l
      | `O l :: r -> obj [`Os] (fun l -> arr (List.rev_append l acc) k r) l
    and obj acc k = function
      | [] -> k (List.rev (`Oe :: acc))
      | (n, x) :: r ->
          base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x
    and base k = function
      | `A l -> arr [`As] k l
      | `O l -> obj [`Os] k l
      | #value as x -> k [x]
    in
    base (fun l -> l) json
  in
  let rec write k = function
    | `Ok -> k ()
    | `Partial ->
        flusher (Jsonm.Manual.dst_rem encoder) ;
        write k (Jsonm.encode encoder `Await)
  in
  let rec go k = function
    | [] -> write k (Jsonm.encode encoder `End)
    | lexeme :: r ->
        write (fun () -> go k r) (Jsonm.encode encoder (`Lexeme lexeme))
  in
  let lexemes = flat_json json in
  go (fun x -> x) lexemes

let json_of_string x =
  (* XXX(dinosaure): [Jsonm] does not reach [`Await]/[refiller] case if input is
     [`String]. *)
  json_of_input (fun () -> assert false) (`String x)

let json_to_string x =
  let buf = Buffer.create 0x100 in
  (* XXX(dinosaure): [Jsonm] does not reach [`Partial]/[flusher] case if output
     os [`Buffer]. *)
  json_to_output (fun _ -> assert false) (`Buffer buf) x ;
  Buffer.contents buf

let tests =
  [ `A [`Bool true]
  ; `O [("a", `A [`Bool true; `Bool false])]
  ; `A [`O [("a", `Bool true); ("b", `Bool false)]] ]

let json = Alcotest.testable (Fmt.using json_to_string Fmt.string) ( = )

let make v =
  Alcotest.test_case (json_to_string v) `Quick
  @@ fun () ->
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

  let encoder = Encoder.create
      ~emitter
      ~margin:78
      ~new_line:"\n" 0x100 in

  let kend encoder =
    if Encoder.is_empty encoder
    then ()
    else Fmt.failwith "Leave a non-empty encoder" in

  let () = Encoder.keval kend encoder Encoder.[!!value; new_line] v in
  let res = Buffer.contents buf in
  let res = json_of_string res in
  Alcotest.(check json) "encode:decode:compare" v res

let unroll_box () =
  Alcotest.test_case "unroll" `Quick @@ fun () ->
  let g ppf (k, x, y) = let open Encoder in eval ppf [ !!string; char $ ':'; tbox 1; !!string; cut; char $ '&'; cut; !!string; close; new_line ] k x y in
  let result = Encoder.to_string ~margin:14 g ("AAAAAA", "BBBBBB", "CCCCCC") in
  Alcotest.(check string) "result" result "AAAAAA:\r\n  BBBBBB&CCCCCC\r\n"
    (* XXX(dinosaure): instead to cut at [&], it unroll the [tbox] and print elements to the new line. *)

let () = Alcotest.run "format" [ "json", List.map make tests
                               ; "unroll", [ unroll_box () ] ]
