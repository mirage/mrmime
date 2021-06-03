open Cmdliner

(** Fortuna command *)
let fortuna = Generate_fortuna.generate

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

let output = Arg.(value & pos ~rev:true 0 filename `Standard & info [])

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
let crowbar dst = Generate_crowbar.generate dst

let int64 =
  Arg.conv
    ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)

let seed64 =
  let doc = "Crowbar seed." in
  Arg.(value & opt (some int64) None & info [ "s"; "seed" ] ~doc)

let crowbar_cmd =
  let doc = "Generate a randomly generated valid email." in
  let man =
    [
      `S "DESCRIPTION"; `P "Generate a random email using $(i,crowbar) fuzzer.";
    ]
  in
  (Term.(ret (const crowbar $ seed64 $ output)), Term.info "crowbar" ~doc ~man)

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
let () = Term.(exit_status @@ eval_choice default_cmd cmds)
