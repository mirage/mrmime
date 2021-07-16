open Cmdliner

let empty_mail = (Mrmime.Header.empty, Mrmime.(Mail.Leaf ""))

let parse_and_compare ~debug mail =
  let str_mail =
    Utils.(mail_to_mt mail)
    |> Mrmime.Mt.to_stream
    |> Utils.buffer_stream_to_string
  in
  match Angstrom.parse_string ~consume:All Mrmime.Mail.mail str_mail with
  | Ok mail' ->
      if Equality.equal ~debug mail mail' then (
        if debug then
          Format.printf "Parsed mail and generated mail are equal.\n";
        (str_mail, `Ok 0))
      else (
        if debug then (
          Format.printf "Parsed mail is not equal to generated mail.\n";
          Format.printf "\nStruture of generated mail\n";
          Utils.print_struct ~debug mail;
          Format.printf "\nStruture of parsed mail\n";
          Utils.print_struct ~debug mail';
          Format.printf "\n++++++++ Generated mail ++++++++\n\n";
          Utils.print_mail mail;
          Format.printf "\n\n++++++++ Parsed mail ++++++++\n\n";
          Utils.print_mail mail');
        failwith "not equal")
  | Error s ->
      if debug then Format.printf "Generated mail can't be parsed.\n";
      (str_mail, `Error (false, s))

let crowbar_mail_generator ?(debug = false) seed multi dst input =
  let module Generate = Fuzz.Make (Crowbar_fuzz) in
  let open Crowbar_fuzz in
  let mail, ret = (ref "", ref (`Ok 0)) in
  (* Note: for a test to be a failure for afl, an exception must be
     raised by the function called by [AflPersistant.run] so in the
     following [test] function here. *)
  let test =
    Test
      ( "mail",
        [ Generate.mail ],
        fun m ->
          let m, r = parse_and_compare ~debug m in
          (match r with
          | `Ok _ -> ( match dst with `Dir _ -> Utils.print dst m | _ -> ())
          | `Error _ -> if debug then Utils.(print dst m));
          mail := m;
          ret := r )
  in
  match multi with
  | None ->
      Crowbar_fuzz.run_one_test seed 1 input [] test;
      (!mail, !ret)
  | Some m ->
      let seed = match seed with None -> Random.int64 1000L | Some n -> n in
      for i = 0 to m do
        let seed = Int64.add (Int64.of_int i) seed in
        if debug then Format.printf "\nSeed %s: " (Int64.to_string seed);
        Crowbar_fuzz.run_one_test (Some seed) 1 input [] test
      done;
      (!mail, !ret)

let fortuna_mail_generator ?(debug = false) g =
  let module Generate = Fuzz.Make (Fortuna) in
  assert (Mirage_crypto_rng.Fortuna.seeded ~g);
  let mail = Fortuna.run ~g Generate.mail in
  parse_and_compare ~debug mail

let generate ~debug (seed : [ `Crowbar of int64 option | `Fortuna of string ])
    multi dst input =
  let _, ret =
    match seed with
    | `Crowbar s ->
        Random.self_init ();
        crowbar_mail_generator ~debug s multi dst input
    | `Fortuna s ->
        let g = Mirage_crypto_rng.Fortuna.create () in
        Mirage_crypto_rng.Fortuna.reseed ~g (Cstruct.of_string s);
        fortuna_mail_generator ~debug g
  in
  ret

(* Cmdliner function s*)

(** Common arguments *)
let multi =
  let doc = "Enables to generate $(docv) random mails." in
  Arg.(value & opt (some int) None & info [ "m"; "multi" ] ~doc ~docv:"MULTI")

let output =
  let filename =
    let parser = function
      | "-" -> Ok `Standard
      | str -> Rresult.(Fpath.of_string str >>| fun v -> `Dir v)
    in
    let pp ppf = function
      | `Standard -> Fmt.string ppf "-"
      | `Dir v -> Fpath.pp ppf v
    in
    Arg.conv (parser, pp)
  in
  let doc =
    "Output directory. The filename is automatically generated. The output is \
     stdout by default."
  in
  Arg.(value & opt filename `Standard & info [ "o"; "output" ] ~doc)

let debug =
  let doc =
    "Print information for debugging. Should not be used in afl mode."
  in
  Arg.(value & flag & info [ "v"; "debug" ] ~doc)

(** Fortuna command *)
let fortuna debug seed output = generate ~debug (`Fortuna seed) None output None

let base64 =
  Arg.conv
    ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)

let seed =
  let doc = "Fortuna seed." in
  Arg.(required & opt (some base64) None & info [ "s"; "seed" ] ~doc)

let fortuna_cmd =
  let doc = "Generate a randomly generated valid email from a seed." in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "Generate a random email from the $(i,fortuna) random number generator \
         and the $(i,base64) given seed.";
    ]
  in
  ( Term.(ret (const fortuna $ debug $ seed $ output)),
    Term.info "fortuna" ~doc ~man )

(** Crowbar command*)
let crowbar debug seed multi dst input =
  generate ~debug (`Crowbar seed) multi dst input

let int64 =
  Arg.conv
    ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)

let seed64 =
  let doc = "Crowbar seed." in
  Arg.(value & opt (some int64) None & info [ "s"; "seed" ] ~doc)

let randomness_file =
  let doc = "Source mail for afl." in
  Cmdliner.Arg.(
    value & pos ~rev:true 0 (some file) None & info [] ~doc ~docv:"FILE")

let crowbar_cmd =
  let doc = "Generate a randomly generated valid email." in
  let man =
    [
      `S "DESCRIPTION"; `P "Generate a random email using $(i,crowbar) fuzzer.";
    ]
  in
  ( Term.(
      ret (const crowbar $ debug $ seed64 $ multi $ output $ randomness_file)),
    Term.info "crowbar" ~doc ~man )

let default_cmd =
  let man =
    [
      `S "DESCRIPTION";
      `P
        "Generate a random email using $(i,crowbar) fuzzer or $(i,fortuna) \
         random number generator.";
    ]
  in
  let doc = "a random mails generator" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = man in
  ( Term.(ret (const (`Help (`Pager, None)))),
    Term.info "generate" ~doc ~sdocs ~exits ~man )

let cmds = [ fortuna_cmd; crowbar_cmd ]
let () = Term.(exit_status @@ eval_choice ~catch:false default_cmd cmds)
