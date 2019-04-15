let () = Printexc.record_backtrace true

open Cmdliner

let commands = [ Header.command; Extract.command ]

let run = `Help (`Pager, None)

let command =
  let doc = "Mrmime tool." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Mrmime tool" ] in
  Term.(ret (const run)), Term.info "mrmime" ~doc ~exits ~man

let () =
  Term.(exit @@ eval_choice command commands)
