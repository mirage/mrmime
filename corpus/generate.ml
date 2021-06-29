open Cmdliner

let empty_mail = (Mrmime.Header.empty, Mrmime.(Mail.Leaf ""))

(** exception must be raised inside the "AflPersistant.run" function *)
let parse_and_compare ~verbose mail =
  let str_mail =
    Utils.(mail_to_mt mail |> Mrmime.Mt.to_stream |> buffer_stream_to_string)
  in
  match Angstrom.parse_string ~consume:All Mrmime.Mail.mail str_mail with
  | Ok mail' ->
      if Equality.equal mail mail' then (str_mail, `Ok 0)
      else (
        if verbose then (
          Format.printf "Parsed mail is not equal to generated mail.\n";
          Format.printf "\nStruture of generated mail\n";
          Utils.print_struct ~verbose:true mail;
          Format.printf "\nStruture of parsed mail\n";
          Utils.print_struct ~verbose:true mail';
          Format.printf "\n++++++++ Generated mail ++++++++\n\n";
          Utils.print_mail mail;
          Format.printf "\n\n++++++++ Parsed mail ++++++++\n\n";
          Utils.print_mail mail');
        failwith "not equal")
  | Error s ->
      if verbose then Format.printf "\nGenerated mail can't be parsed.\n";
      (str_mail, `Error (false, s))

(* Multi is only defined for debug right now *)
let crowbar_mail_generator ?(verbose = false) seed input multi =
  let module Generate = Fuzz.Make (Crowbar_fuzz) in
  let open Crowbar_fuzz in
  let mail, ret = (ref "", ref (`Ok 0)) in
  let test =
    Test
      ( "mail",
        [ Generate.mail ],
        fun m ->
          let a = parse_and_compare ~verbose m in
          mail := fst a;
          ret := snd a )
  in
  match multi with
  | None ->
      Crowbar_fuzz.run_one_test seed 1 input [] test;
      (!mail, !ret)
  | Some m ->
      let seed = match seed with None -> Random.int64 1000L | Some n -> n in
      for i = 0 to m do
        let seed = Int64.add (Int64.of_int i) seed in
        if verbose then Format.printf "Seed: %s@." (Int64.to_string seed);
        Crowbar_fuzz.run_one_test (Some seed) 1 input [] test
      done;
      (!mail, !ret)

let fortuna_mail_generator ?(verbose = false) g =
  let module Generate = Fuzz.Make (Fortuna) in
  assert (Mirage_crypto_rng.Fortuna.seeded ~g);
  let mail = Fortuna.run ~g Generate.mail in
  parse_and_compare ~verbose mail

let generate ~verbose (seed : [ `Crowbar of int64 option | `Fortuna of string ]) multi
    dst input =
  let mail, ret =
    match seed with
    | `Crowbar s ->
        Random.self_init ();
        crowbar_mail_generator ~verbose s input multi
    | `Fortuna s ->
        let g = Mirage_crypto_rng.Fortuna.create () in
        Mirage_crypto_rng.Fortuna.reseed ~g (Cstruct.of_string s);
        fortuna_mail_generator ~verbose g
  in
  (match ret with `Error (_, _) -> Utils.print dst mail | _ -> ());
  ret


(** Commun arguments *)
let multi =
  let doc = "Debug." in
  Arg.(value & opt (some int) None & info [ "m"; "multi" ] ~doc)

let filename =
  let parser = function
    | "-" -> Ok `Standard
    | str -> Rresult.(Fpath.of_string str >>| fun v -> `Filename v)
  in
  let pp ppf = function
    | `Standard -> Fmt.string ppf "-"
    | `Filename v -> Fpath.pp ppf v
  in
  Arg.conv (parser, pp)

let output =
  let doc = "Output file, standard by default." in
  Arg.(value & opt filename `Standard & info [ "o"; "output" ] ~doc)

let verbose =
  let doc = "Debug." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

(** Fortuna command *)
let fortuna verbose seed output = generate ~verbose(`Fortuna seed) None output None

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
  (Term.(ret (const fortuna $ verbose $ seed $ output)), Term.info "fortuna" ~doc ~man)

(** Crowbar command*)
let crowbar verbose seed multi dst input = generate ~verbose (`Crowbar seed) multi dst input

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
  ( Term.(ret (const crowbar $ verbose $ seed64 $ multi $ output $ randomness_file)),
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
