module Gen = Fuzz.Make (Crowbar_fuzz)
open Utils

let tests () =
  Crowbar.add_test ~name:"test" [ Gen.date ] (fun d ->
      Format.printf "%s" (date_to_string d))


let generate dst =
  Crowbar.add_test ~name:"mail generation" [ Gen.mail ] (fun mail ->
      let stream = Mrmime.Mt.to_stream mail in
      let str = stream_to_string stream in
      let res =
        match Angstrom.parse_string ~consume:All Mrmime.Mail.mail str with
        | Ok _ -> `Ok 0
        | Error _ ->
            Fmt.epr "Invalid mail: @[<hov>%a@]\n%!"
              (Hxd_string.pp Hxd.default)
              str;
            assert false
      in
      let oc, oc_close = build_oc dst in
      output_string oc str;
      oc_close oc;
      Crowbar.check_eq res (`Ok 0));
  `Ok 0
