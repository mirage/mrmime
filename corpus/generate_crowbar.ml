module Generate = Fuzz.Make (Crowbar_fuzz)

(* [./generate.exe crowbar] command line calls this function *)
let generate seed dst =
  Random.self_init ();
  let seed = match seed with None -> Random.int64 1000L | Some s -> s in
  let res = ref (`Ok 1) in
  let maxrun = 1000 in
  let rec go seed = function
    | 0 -> `Ok 0
    | nbrun ->
        Format.printf "Seed : %s@." (Int64.to_string seed);
        let str = ref "" in
        let valid = ref true in
        let tests =
          [
            Crowbar_fuzz.Test
              ( "mail",
                [ Generate.mail ],
                fun mail ->
                  let stream = Mrmime.Mt.to_stream mail in
                  str := Utils.buffer_stream_to_string stream;
                  res :=
                    match
                      Angstrom.parse_string ~consume:All Mrmime.Mail.mail !str
                    with
                    | Ok _ -> `Ok 0
                    | Error s ->
                        valid := false;
                        `Error (false, s)
                    | exception exc ->
                        valid := false;
                        raise exc );
          ]
        in
        let _nbfailures = Crowbar_fuzz.run_all_tests (Some seed) 1 tests in
        if not !valid then (
          let oc, oc_close = Utils.build_oc dst in
          Format.printf "INVALID MAIL :@.";
          output_string oc !str;
          oc_close oc;
          !res)
        else go (Int64.add seed 1L) (nbrun - 1)
  in
  go seed maxrun
