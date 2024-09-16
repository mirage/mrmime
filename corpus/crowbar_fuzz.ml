(* This code mainly come from
   https://github.com/stedolan/crowbar/blob/master/src/crowbar.ml. *)

type src = Random of Random.State.t | Fd of Unix.file_descr

type state =
  { chan : src; buf : Bytes.t; mutable offset : int; mutable len : int }

type 'a printer = Format.formatter -> 'a -> unit

type 'a strat =
  | Choose of 'a t list
  | Map : ('f, 'a) gens * 'f -> 'a strat
  | Bind : 'a t * ('a -> 'b t) -> 'b strat
  | Option : 'a t -> 'a option strat
  | List : 'a t -> 'a list strat
  | List1 : 'a t -> 'a list strat
  | Unlazy of 'a t Lazy.t
  | Primitive of (state -> 'a)
  | Print of 'a printer * 'a t

and 'a t = { strategy : 'a strat; small_examples : 'a list }

and ('k, 'res) gens =
  | [] : ('res, 'res) gens
  | ( :: ) : 'a t * ('k, 'res) gens -> ('a -> 'k, 'res) gens

type nonrec +'a list = 'a list = [] | ( :: ) of 'a * 'a list

let unlazy f = { strategy = Unlazy f; small_examples = [] }

let fix f =
  let rec lazygen = lazy (f (unlazy lazygen)) in
  unlazy lazygen

let map (type f a) (gens : (f, a) gens) (f : f) =
  { strategy = Map (gens, f);
    small_examples = (match gens with [] -> [ f ] | _ -> [])
  }

let dynamic_bind m f = { strategy = Bind (m, f); small_examples = [] }
let ( >>= ) = dynamic_bind
let const x = map [] x

let choose gens =
  { strategy = Choose gens;
    small_examples = List.map (fun x -> x.small_examples) gens |> List.concat
  }

let option gen = { strategy = Option gen; small_examples = [ None ] }
let list gen = { strategy = List gen; small_examples = [ [] ] }

let list1 gen =
  { strategy = List1 gen;
    small_examples = List.map (fun x -> [ x ]) gen.small_examples
  }

let primitive f ex = { strategy = Primitive f; small_examples = [ ex ] }
let pair gena genb = map [ gena; genb ] (fun a b -> (a, b))

let concat ~sep l =
  match l with
  | h :: t ->
      List.fold_left
        (fun acc e -> map [ acc; sep; e ] (fun acc sep e -> acc ^ sep ^ e))
        h t
  | [] -> const ""

let with_printer pp gen =
  { strategy = Print (pp, gen); small_examples = gen.small_examples }

let pp = Format.fprintf
let pp_int ppf n = pp ppf "%d" n
let pp_int32 ppf n = pp ppf "%s" (Int32.to_string n)
let pp_int64 ppf n = pp ppf "%s" (Int64.to_string n)
let pp_float ppf f = pp ppf "%f" f
let pp_bool ppf b = pp ppf "%b" b
let pp_char ppf c = pp ppf "%c" c
let pp_string ppf s = pp ppf "\"%s\"" (String.escaped s)

let pp_list pv ppf l =
  pp ppf "@[<hv 1>[%a]@]"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> pp ppf ";@ ") pv)
    l

let pp_option pv ppf = function
  | None -> Format.fprintf ppf "None"
  | Some x -> Format.fprintf ppf "(Some %a)" pv x

exception BadTest of string
exception FailedTest of unit printer

let guard = function true -> () | false -> raise (BadTest "guard failed")
let bad_test _ = raise (BadTest "bad test")

let get_data chan buf off len =
  match chan with
  | Random rand ->
      for i = off to off + len - 1 do
        Bytes.set buf i (Char.chr (Random.State.bits rand land 0xff))
      done;
      len - off
  | Fd ch -> Unix.read ch buf off len

let refill src =
  assert (src.offset <= src.len);
  let remaining = src.len - src.offset in
  (* move remaining data to start of buffer *)
  Bytes.blit src.buf src.offset src.buf 0 remaining;
  src.len <- remaining;
  src.offset <- 0;
  let read =
    get_data src.chan src.buf remaining (Bytes.length src.buf - remaining)
  in
  if read = 0 then raise (BadTest "premature end of file")
  else src.len <- remaining + read

let rec getbytes src n =
  assert (src.offset <= src.len);
  if n > Bytes.length src.buf then failwith "request too big";
  if src.len - src.offset >= n then (
    let off = src.offset in
    src.offset <- src.offset + n;
    off)
  else (
    refill src;
    getbytes src n)

let read_char src =
  let off = getbytes src 1 in
  Bytes.get src.buf off

let read_byte src = Char.code (read_char src)

let read_bool src =
  let n = read_byte src in
  n land 1 = 1

let bool = with_printer pp_bool (primitive read_bool false)

let read_int32 src =
  let off = getbytes src 4 in
  EndianBytes.LittleEndian.get_int32 src.buf off

let read_int64 src =
  let off = getbytes src 8 in
  EndianBytes.LittleEndian.get_int64 src.buf off

let int32 = with_printer pp_int32 (primitive read_int32 0l)
let int64 = with_printer pp_int64 (primitive read_int64 0L)

let int =
  with_printer pp_int
    (if Sys.word_size <= 32 then map [ int32 ] Int32.to_int
     else map [ int64 ] Int64.to_int)

let float =
  with_printer pp_float
    (primitive
       (fun src ->
         let off = getbytes src 8 in
         EndianBytes.LittleEndian.get_double src.buf off)
       0.)

let char = with_printer pp_char (primitive read_char 'a')

(* maybe print as a hexdump? *)
let string =
  with_printer pp_string
    (primitive
       (fun src ->
         (* null-terminated, with '\001' as an escape code *)
         let buf = Bytes.make 64 '\255' in
         let rec read_bytes p =
           if p >= Bytes.length buf then p
           else
             match read_char src with
             | '\000' -> p
             | '\001' ->
                 Bytes.set buf p (read_char src);
                 read_bytes (p + 1)
             | c ->
                 Bytes.set buf p c;
                 read_bytes (p + 1)
         in
         let count = read_bytes 0 in
         Bytes.sub_string buf 0 count)
       "")

let fixed n =
  with_printer pp_string
    (primitive
       (fun src ->
         let off = getbytes src n in
         Bytes.sub_string src.buf off n)
       (String.make n 'a'))

let choose_int n state =
  assert (n > 0);
  if n = 1 then 0
  else if n <= 0x100 then read_byte state mod n
  else if n < 0x1000000 then
    Int32.(to_int (abs (rem (read_int32 state) (of_int n))))
  else Int64.(to_int (abs (rem (read_int64 state) (of_int n))))

let range ?(min = 0) n =
  assert (n > 0);
  if n <= 0 then
    raise (Invalid_argument "Crowbar.range: argument n must be positive");
  if min < 0 then
    raise
      (Invalid_argument "Crowbar.range: argument min must be positive or null");
  with_printer pp_int (primitive (fun s -> min + choose_int n s) min)

exception GenFailed of exn * Printexc.raw_backtrace * unit printer

let rec generate : type a. int -> state -> a t -> a * unit printer =
 fun size input gen ->
  (* if size <= 1 && gen.small_examples <> [] then
       (List.hd gen.small_examples, fun ppf () -> pp ppf "?")
     else *)
  match gen.strategy with
  | Choose gens ->
      (* FIXME: better distribution? *)
      (* FIXME: choices of size > 255? *)
      let n = choose_int (List.length gens) input in
      let v, pv = generate size input (List.nth gens n) in
      (v, fun ppf () -> pp ppf "#%d %a" n pv ())
  | Map ([], k) -> (k, fun ppf () -> pp ppf "?")
  | Map (gens, f) -> (
      let rec len : type k res. int -> (k, res) gens -> int =
       fun acc xs -> match xs with [] -> acc | _ :: xs -> len (1 + acc) xs
      in
      let n = len 0 gens in
      (* the size parameter is (apparently?) meant to ensure that generation
         eventually terminates, by limiting the set of options from which the
         generator might choose once we've gotten deep into a tree.  make sure we
         always mark our passing, even when we've mapped one value into another,
         so we don't blow the stack. *)
      let size = (size - 1) / n in
      let v, pvs = gen_apply size input gens f in
      match v with
      | Ok v -> (v, pvs)
      | Error (e, bt) -> raise (GenFailed (e, bt, pvs)))
  | Bind (m, f) ->
      let index, pv_index = generate (size - 1) input m in
      let a, pv = generate (size - 1) input (f index) in
      (a, fun ppf () -> pp ppf "(%a) => %a" pv_index () pv ())
  | Option gen ->
      if size < 1 then (None, fun ppf () -> pp ppf "None")
      else if read_bool input then
        let v, pv = generate size input gen in
        (Some v, fun ppf () -> pp ppf "Some (%a)" pv ())
      else (None, fun ppf () -> pp ppf "None")
  | List gen ->
      let elems = generate_list size input gen in
      ( List.map fst elems,
        fun ppf () -> pp_list (fun ppf (_, pv) -> pv ppf ()) ppf elems )
  | List1 gen ->
      let elems = generate_list1 size input gen in
      ( List.map fst elems,
        fun ppf () -> pp_list (fun ppf (_, pv) -> pv ppf ()) ppf elems )
  | Primitive gen -> (gen input, fun ppf () -> pp ppf "?")
  | Unlazy gen -> generate size input (Lazy.force gen)
  | Print (ppv, gen) ->
      let v, _ = generate size input gen in
      (v, fun ppf () -> ppv ppf v)

and generate_list : type a. int -> state -> a t -> (a * unit printer) list =
 fun size input gen ->
  if size <= 1 then []
  else if read_bool input then generate_list1 size input gen
  else []

and generate_list1 : type a. int -> state -> a t -> (a * unit printer) list =
 fun size input gen ->
  let ans = generate (size / 2) input gen in
  ans :: generate_list (size / 2) input gen

and gen_apply :
    type k res.
    int ->
    state ->
    (k, res) gens ->
    k ->
    (res, exn * Printexc.raw_backtrace) result * unit printer =
 fun size state gens f ->
  let rec go :
      type k res.
      int ->
      state ->
      (k, res) gens ->
      k ->
      (res, exn * Printexc.raw_backtrace) result * unit printer list =
   fun size input gens ->
    match gens with
    | [] -> fun x -> (Ok x, [])
    | g :: gs ->
        fun f ->
          let v, pv = generate size input g in
          let res, pvs =
            match f v with
            | exception (BadTest _ as e) -> raise e
            | exception e -> (Error (e, Printexc.get_raw_backtrace ()), [])
            | fv -> go size input gs fv
          in
          (res, pv :: pvs)
  in
  let v, pvs = go size state gens f in
  let pvs ppf () =
    match pvs with
    | [ pv ] -> pv ppf ()
    | pvs -> pp_list (fun ppf pv -> pv ppf ()) ppf pvs
  in
  (v, pvs)

let () = Printexc.record_backtrace true

type test = Test : string * ('f, unit) gens * 'f -> test

type test_status =
  | TestPass of unit printer
  | BadInput of string
  | GenFail of exn * Printexc.raw_backtrace * unit printer
  | TestExn of exn * Printexc.raw_backtrace * unit printer
  | TestFail of unit printer * unit printer

let run_once (gens : (_, unit) gens) f state =
  match gen_apply 100 state gens f with
  | Ok (), pvs -> TestPass pvs
  | Error (FailedTest p, _), pvs -> TestFail (p, pvs)
  | Error (e, bt), pvs -> TestExn (e, bt, pvs)
  | exception BadTest s -> BadInput s
  | exception GenFailed (e, bt, pvs) -> GenFail (e, bt, pvs)

let classify_status = function
  | TestPass _ -> `Pass
  | BadInput _ -> `Bad
  | GenFail _ -> `Fail (* slightly dubious... *)
  | TestExn _ | TestFail _ -> `Fail

let print_status ppf status =
  let print_ex ppf (e, bt) =
    pp ppf "%s" (Printexc.to_string e);
    bt
    |> Printexc.raw_backtrace_to_string
    |> Str.split (Str.regexp "\n")
    |> List.iter (pp ppf "@,%s")
  in
  match status with
  | TestPass pvs ->
      pp ppf "When given the input:@.@[<v 4>@,%a@,@]@.the test passed." pvs ()
  | BadInput s -> pp ppf "The testcase was invalid:@.%s" s
  | GenFail (e, bt, pvs) ->
      pp ppf
        "When given the input:@.@[<4>%a@]@.the testcase generator threw an \
         exception:@.@[<v 4>@,\
         %a@,\
         @]"
        pvs () print_ex (e, bt)
  | TestExn (e, bt, pvs) ->
      pp ppf
        "When given the input:@.@[<v 4>@,\
         %a@,\
         @]@.the test threw an exception:@.@[<v 4>@,\
         %a@,\
         @]"
        pvs () print_ex (e, bt)
  | TestFail (err, pvs) ->
      pp ppf
        "When given the input:@.@[<v 4>@,\
         %a@,\
         @]@.the test failed:@.@[<v 4>@,\
         %a@,\
         @]"
        pvs () err ()

let prng_state_of_seed seed =
  (* try to make this independent of word size *)
  let seed =
    Int64.
      [| to_int (logand (of_int 0xffff) seed);
         to_int (logand (of_int 0xffff) (shift_right seed 16));
         to_int (logand (of_int 0xffff) (shift_right seed 32));
         to_int (logand (of_int 0xffff) (shift_right seed 48))
      |]
  in
  Random.State.make seed

let src_of_seed seed = Random (prng_state_of_seed seed)

let run_test ~mode ~silent ?(verbose = false) (Test (name, gens, f)) =
  let show_status_line ?(clear = false) stat =
    Printf.printf "%s: %s\n" name stat;
    if clear then print_newline ();
    flush stdout
  in
  let ppf = Format.std_formatter in
  if (not silent) && Unix.isatty Unix.stdout then
    show_status_line ~clear:false "....";
  let status =
    match mode with
    | `Once state -> run_once gens f state
    | `Repeat (iters, seedseed) ->
        let worst_status = ref (TestPass (fun _ () -> ())) in
        let npass = ref 0 in
        let nbad = ref 0 in
        let seedsrc = prng_state_of_seed seedseed in
        while !npass < iters && classify_status !worst_status = `Pass do
          let seed = Random.State.int64 seedsrc Int64.max_int in
          let state =
            { chan = src_of_seed seed;
              buf = Bytes.make 256 '0';
              offset = 0;
              len = 0
            }
          in
          let status = run_once gens f state in
          match classify_status status with
          | `Pass -> incr npass
          | `Bad -> incr nbad
          | `Fail -> worst_status := status
        done;
        let status = !worst_status in
        status
  in
  if silent && verbose && classify_status status = `Fail then (
    show_status_line ~clear:true "FAIL";
    pp ppf "%a@." print_status status);
  (if not silent then
     match classify_status status with
     | `Pass ->
         show_status_line ~clear:true "PASS";
         if verbose then pp ppf "%a@." print_status status
     | `Fail ->
         show_status_line ~clear:true "FAIL";
         pp ppf "%a@." print_status status
     | `Bad ->
         show_status_line ~clear:true "BAD";
         pp ppf "%a@." print_status status);
  status

exception TestFailure

let run_one_test seed repeat file verbosity test =
  match file with
  | None -> (
      let seed =
        match seed with Some seed -> seed | None -> Random.int64 Int64.max_int
      in
      let status = run_test ~mode:(`Repeat (repeat, seed)) ~silent:true test in
      match status with
      | TestFail _ -> raise TestFailure
      | GenFail (e, _, _) -> raise e
      | _ -> ())
  | Some file ->
      (* AFL mode *)
      let verbose = List.length verbosity > 0 in
      let () =
        AflPersistent.run (fun () ->
            let fd = Unix.openfile file [ Unix.O_RDONLY ] 0o000 in
            let state =
              { chan = Fd fd; buf = Bytes.make 256 '0'; offset = 0; len = 0 }
            in
            let status =
              try run_test ~mode:(`Once state) ~silent:true ~verbose test
              with BadTest s -> BadInput s
            in
            Unix.close fd;
            match classify_status status with
            | `Pass | `Bad -> ()
            | `Fail ->
                Printexc.record_backtrace false;
                raise TestFailure)
      in
      ()
