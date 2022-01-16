let comma = ((fun t () -> Prettym.char t ','), ())

let rec value t x =
  let binding t (k, v) =
    Prettym.eval t
      Prettym.[ char $ '"'; !!string; char $ '"'; char $ ':'; !!value ]
      k v
  in
  let arr = Prettym.list ~sep:comma value in
  let obj = Prettym.list ~sep:comma binding in
  match x with
  | `Bool true -> Prettym.string t "true"
  | `Bool false -> Prettym.string t "false"
  | `Null -> Prettym.string t "null"
  | `Float f -> Prettym.string t (Fmt.str "%.16g" f)
  | `String s -> Prettym.eval t Prettym.[ char $ '"'; !!string; char $ '"' ] s
  | `A a -> Prettym.eval t Prettym.[ char $ '['; !!arr; char $ ']' ] a
  | `O o -> Prettym.eval t Prettym.[ char $ '{'; !!obj; char $ '}' ] o

exception Fail

let json =
  let open Crowbar in
  let valid str =
    let is = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
      | _ -> false
    in
    try
      String.iter (fun chr -> if not (is chr) then raise Fail) str;
      true
    with Fail -> false
  in

  fix @@ fun m ->
  let string = map [ bytes ] (fun x -> if valid x then x else bad_test ()) in
  let binding = map [ string; m ] (fun k v -> (k, v)) in
  choose
    [ const `Null;
      map [ bool ] (fun x -> `Bool x);
      map [ string ] (fun x -> `String x);
      map [ list m ] (fun x -> `A x);
      map [ list binding ] (fun x -> `O x)
    ]

type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]
type value = [ `Null | `Bool of bool | `String of string | `Float of float ]

let json_of_input refiller input =
  let decoder = Jsonm.decoder input in

  let error (`Error err) = Fmt.invalid_arg "%a" Jsonm.pp_error err in
  let end_of_input `End = Fmt.invalid_arg "Unexpected end of input" in

  let rec arr acc k =
    match Jsonm.decode decoder with
    | #await ->
        refiller ();
        arr acc k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> base (fun v -> arr (v :: acc) k) v
  and name n k =
    match Jsonm.decode decoder with
    | #await ->
        refiller ();
        name n k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme v -> base (fun v -> k (n, v)) v
  and obj acc k =
    match Jsonm.decode decoder with
    | #await ->
        refiller ();
        obj acc k
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
    | #await ->
        refiller ();
        go k
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
      | `A l :: r -> arr [ `As ] (fun l -> arr (List.rev_append l acc) k r) l
      | `O l :: r -> obj [ `Os ] (fun l -> arr (List.rev_append l acc) k r) l
    and obj acc k = function
      | [] -> k (List.rev (`Oe :: acc))
      | (n, x) :: r ->
          base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x
    and base k = function
      | `A l -> arr [ `As ] k l
      | `O l -> obj [ `Os ] k l
      | #value as x -> k [ x ]
    in

    base (fun l -> l) json
  in

  let rec write k = function
    | `Ok -> k ()
    | `Partial ->
        flusher (Jsonm.Manual.dst_rem encoder);
        write k (Jsonm.encode encoder `Await)
  in

  let rec go k = function
    | [] -> write k (Jsonm.encode encoder `End)
    | lexeme :: r ->
        write (fun () -> go k r) (Jsonm.encode encoder (`Lexeme lexeme))
  in

  let lexemes = flat_json json in

  go (fun x -> x) lexemes

let json_of_string x = json_of_input (fun () -> assert false) (`String x)

let json_to_string x =
  let buf = Buffer.create 0x100 in
  json_to_output (fun _ -> assert false) (`Buffer buf) x;
  Buffer.contents buf

let rec pp_json ppf = function
  | `Null -> Fmt.string ppf "<null>"
  | `Bool v -> Fmt.bool ppf v
  | `Float v -> Fmt.float ppf v
  | `String v -> Fmt.string ppf v
  | `A v -> Fmt.Dump.list pp_json ppf v
  | `O v -> Fmt.(Dump.list Dump.(pair string pp_json)) ppf v

let rec list_cmp cmp a b =
  match (a, b) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      let n = cmp x y in
      if n = 0 then list_cmp cmp xs ys else n

let cmp_bool a b =
  match (a, b) with
  | true, true | false, false -> 0
  | true, false -> 1
  | false, true -> -1

let rec cmp_json a b =
  match (a, b) with
  | `Null, `Null -> 0
  | `Bool a, `Bool b -> cmp_bool a b
  | `String a, `String b -> String.compare a b
  | `Float a, `Float b -> Stdlib.Float.compare a b
  | `A a, `A b -> list_cmp cmp_json a b
  | `O a, `O b ->
      let cmp (ka, a) (kb, b) =
        let x = String.compare ka kb in
        if x = 0 then cmp_json a b else x
      in
      list_cmp cmp a b
  | `Null, _ -> -1
  | _, `Null -> 1
  | `Bool _, _ -> -1
  | _, `Bool _ -> 1
  | `String _, _ -> -1
  | _, `String _ -> 1
  | `Float _, _ -> -1
  | _, `Float _ -> 1
  | `A _, _ -> -1
  | _, `A _ -> 1
  | `O _, _ -> -1
  | _, `O _ -> 1
  | `AnyOtherTag, `AnyOtherTag -> assert false

let eq_json a b = cmp_json a b = 0

module BBuffer = Buffer

let emitter_of_buffer buf =
  let open Prettym in
  let write a = function
    | { IOVec.buffer = Buffer.String x; off; len } ->
        BBuffer.add_substring buf x off len;
        a + len
    | { IOVec.buffer = Buffer.Bytes x; off; len } ->
        BBuffer.add_subbytes buf x off len;
        a + len
    | { IOVec.buffer = Buffer.Bigstring x; off; len } ->
        BBuffer.add_string buf (Bigstringaf.substring x ~off ~len);
        a + len
  in
  List.fold_left write 0

let () =
  Crowbar.add_test ~name:"encoder" [ json ] @@ fun json ->
  let buf = Buffer.create 0x100 in

  let emitter =
    let write a x =
      let open Prettym.IOVec in
      let open Prettym.Buffer in
      match x with
      | { buffer = String x; off; len } ->
          Buffer.add_substring buf x off len;
          a + len
      | { buffer = Bytes x; off; len } ->
          Buffer.add_subbytes buf x off len;
          a + len
      | { buffer = Bigstring x; off; len } ->
          let x = Bigstringaf.substring x ~off ~len in
          Buffer.add_string buf x;
          a + len
    in
    List.fold_left write 0
  in

  let encoder = Prettym.create ~emitter ~margin:78 ~new_line:"\n" 0x100 in
  let encoder = Prettym.eval encoder Prettym.[ !!value; new_line ] json in

  Crowbar.check_eq ~pp:Fmt.bool ~eq:( = ) (Prettym.is_empty encoder) true;

  let res = Buffer.contents buf in
  let res = json_of_string res in

  Crowbar.check_eq ~pp:pp_json ~cmp:cmp_json ~eq:eq_json json res
