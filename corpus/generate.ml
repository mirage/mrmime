open Cmdliner

let empty_mail = (Mrmime.Header.empty, Mrmime.(Mail.Leaf ""))

(** exception must be raised inside the "AflPersistant.run" function *)
let parse_and_compare mail =
  let str_mail =
    Utils.(mail_to_mt mail |> Mrmime.Mt.to_stream |> buffer_stream_to_string)
  in
  match Angstrom.parse_string ~consume:All Mrmime.Mail.mail str_mail with
  | Ok mail' ->
      if Equality.equal (snd mail) (snd mail') then (
        Format.printf "*****Generated mail*****@.";
        Utils.print_struct ~verbose:true mail;
        Format.printf "******Parsed mail*****@.";
        Utils.print_struct ~verbose:true mail';
        (str_mail, `Ok 0))
      else (
        Format.printf "*****Generated mail*****@.";
        Utils.print_struct ~verbose:true mail;
        Utils.print_mail mail;
        Format.printf "******Parsed mail*****@.";
        Utils.print_struct ~verbose:true mail';
        Utils.print_mail mail';
        failwith "not equal")
  | Error s -> (str_mail, `Error (false, s))

let crowbar_mail_generator seed input =
  let module Generate = Fuzz.Make (Crowbar_fuzz) in
  let open Crowbar_fuzz in
  let mail, ret = (ref "", ref (`Ok 0)) in
  let test =
    Test
      ( "mail",
        [ Generate.mail ],
        fun m ->
          let a = parse_and_compare m in
          mail := fst a;
          ret := snd a )
  in
  Crowbar_fuzz.run_one_test seed 1 input [] test;
  (!mail, !ret)

let fortuna_mail_generator g =
  let module Generate = Fuzz.Make (Fortuna) in
  assert (Mirage_crypto_rng.Fortuna.seeded ~g);
  let mail = Fortuna.run ~g Generate.mail in
  parse_and_compare mail

let generate (seed : [ `Crowbar of int64 option | `Fortuna of string ]) dst
    input =
  let mail, ret =
    match seed with
    | `Crowbar s ->
        Random.self_init ();
        crowbar_mail_generator s input
    | `Fortuna s ->
        let g = Mirage_crypto_rng.Fortuna.create () in
        Mirage_crypto_rng.Fortuna.reseed ~g (Cstruct.of_string s);
        fortuna_mail_generator g
  in
  (match ret with `Error (_, _) -> Utils.print dst mail | _ -> ());
  ret

(** Fortuna command *)
let fortuna seed output = generate (`Fortuna seed) output None

let base64 =
  Arg.conv
    ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)

let seed =
  let doc = "Fortuna seed." in
  Arg.(required & opt (some base64) None & info [ "s"; "seed" ] ~doc)

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
  (Term.(ret (const fortuna $ seed $ output)), Term.info "fortuna" ~doc ~man)

(** Crowbar command*)
let crowbar seed dst input = generate (`Crowbar seed) dst input

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
  ( Term.(ret (const crowbar $ seed64 $ output $ randomness_file)),
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
