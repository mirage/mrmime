open Cmdliner

let empty_mail = (Mrmime.Header.empty, Mrmime.(Mail.Leaf ""))

let into_file fpath str =
  let oc = open_out (Fpath.to_string fpath) in
  output_string oc str;
  close_out oc

let save_mails_into_files output a b =
  match output with
  | Some fpath ->
      let fa = Fpath.add_ext "generated" fpath
      and fb = Fpath.add_ext "parsed" fpath in
      into_file fa a;
      into_file fb b
  | None -> ()

let save_mail_into_file output str =
  match output with
  | Some fpath -> into_file (Fpath.add_ext "generated" fpath) str
  | None -> ()

let parse_and_compare ~quiet output mail =
  let str_mail =
    Utils.(mail_to_mt mail)
    |> Mrmime.Mt.to_stream
    |> Utils.buffer_stream_to_string
  in
  match Angstrom.parse_string ~consume:All (Mrmime.Mail.mail None) str_mail with
  | Ok mail' ->
      let str_mail' =
        Utils.(mail_to_mt mail')
        |> Mrmime.Mt.to_stream
        |> Utils.buffer_stream_to_string
      in
      if Equality.equal mail mail' then (
        save_mails_into_files output str_mail str_mail';
        if quiet then `Ok 0
        else (
          Fmt.pr "Parsed mail and generated mail are equal.\n%!";
          `Ok 0))
      else (
        save_mails_into_files output str_mail str_mail';
        `Error (false, "Parsed mail and generared mail are not equal."))
  | Error _ ->
      save_mail_into_file output str_mail;
      `Error (false, "Generated email is invalid.")

let crowbar_mail_generator ~quiet seed multi output input =
  let module Generate = Fuzz.Make (Crowbar_fuzz) in
  let open Crowbar_fuzz in
  let ret = ref (`Ok 0) in
  (* Note: for a test to be a failure for afl, an exception must be
     raised by the function called by [AflPersistant.run] so in the
     following [test] function here. *)
  let test =
    Test
      ( "mail",
        [ Generate.mail ],
        fun m -> ret := parse_and_compare ~quiet output m )
  in
  match multi with
  | None ->
      Crowbar_fuzz.run_one_test seed 1 input [] test;
      !ret
  | Some m ->
      let seed = match seed with None -> Random.int64 1000L | Some n -> n in
      for i = 0 to m - 1 do
        let seed = Int64.add (Int64.of_int i) seed in
        Crowbar_fuzz.run_one_test (Some seed) 1 input [] test
      done;
      !ret

let fortuna_mail_generator ~quiet output g =
  let module Generate = Fuzz.Make (Fortuna) in
  assert (Mirage_crypto_rng.Fortuna.seeded ~g);
  let mail = Fortuna.run ~g Generate.mail in
  parse_and_compare ~quiet output mail

let generate ~quiet (seed : [ `Crowbar of int64 option | `Fortuna of string ])
    multi dst input =
  match seed with
  | `Crowbar s ->
      Random.self_init ();
      crowbar_mail_generator ~quiet s multi dst input
  | `Fortuna s ->
      let g = Mirage_crypto_rng.Fortuna.create () in
      Mirage_crypto_rng.Fortuna.reseed ~g (Cstruct.of_string s);
      fortuna_mail_generator ~quiet dst g

(* Cmdliner function s*)

(** Common arguments *)
let multi =
  let doc = "Enables to generate $(docv) random mails." in
  Arg.(value & opt (some int) None & info [ "m"; "multi" ] ~doc ~docv:"MULTI")

let output =
  let filename = Arg.conv (Fpath.of_string, Fpath.pp) in
  let doc = "Output filename. The filename is automatically generated." in
  Arg.(value & opt (some filename) None & info [ "o"; "output" ] ~doc)

let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Cmd.Env.info "BLAZE_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Cmd.Env.info "BLAZE_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr);
  Option.is_none level

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

(** Fortuna command *)
let fortuna quiet seed output = generate ~quiet (`Fortuna seed) None output None

let base64 =
  Arg.conv
    ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)

let seed =
  let doc = "Fortuna seed." in
  Arg.(required & opt (some base64) None & info [ "s"; "seed" ] ~doc)

let fortuna_cmd =
  let doc = "Generate a randomly generated valid email from a seed." in
  let man =
    [ `S "DESCRIPTION";
      `P
        "Generate a random email from the $(i,fortuna) random number generator \
         and the $(i,base64) given seed."
    ]
  in
  let term = Term.(ret (const fortuna $ setup_logs $ seed $ output))
  and info = Cmd.info "fortuna" ~doc ~man in
  Cmd.v info term

(** Crowbar command*)
let crowbar quiet seed multi dst input =
  generate ~quiet (`Crowbar seed) multi dst input

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
    [ `S "DESCRIPTION";
      `P "Generate a random email using $(i,crowbar) fuzzer."
    ]
  in
  let term =
    Term.(
      ret
        (const crowbar $ setup_logs $ seed64 $ multi $ output $ randomness_file))
  and info = Cmd.info "crowbar" ~doc ~man in
  Cmd.v info term

let default_cmd = Term.(ret (const (`Help (`Pager, None))))
let cmds = [ fortuna_cmd; crowbar_cmd ]

let () =
  let info =
    let man =
      [ `S "DESCRIPTION";
        `P
          "Generate a random email using $(i,crowbar) fuzzer or $(i,fortuna) \
           random number generator."
      ]
    in
    let doc = "a random mails generator" in
    let sdocs = Manpage.s_common_options in
    Cmd.info "generate" ~doc ~sdocs ~man
  in
  let group = Cmd.group ~default:default_cmd info cmds in
  exit (Cmd.eval' ~catch:false group)
