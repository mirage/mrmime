open Mrmime

let romain_calascibetta =
  let open Mailbox in
  Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ])

let gemma =
  let open Mailbox in
  Local.[ w "gemma"; w "gordon" ] @ Domain.(domain, [ a "gmail"; a "com" ])

let anil =
  let open Mailbox in
  Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ])

let richard_mortier =
  let open Mailbox in
  Local.[ w "richard"; w "mortier" ] @ Domain.(domain, [ a "cl"; a "cam"; a "ac"; a "uk" ])

let cambridge =
  let name =
    let open Mailbox.Phrase in
    v [ w "Cambridge"; w "University" ] in
  Group.v ~name [ gemma; anil; richard_mortier ]

let yaron_minsky =
  let open Mailbox in
  Local.[ w "yminsky" ] @ Domain.(domain, [ a "janestreet"; a "com" ])

let date =
  let now = Ptime_clock.now () in
  Option.get_exn (Date.of_ptime ~zone:Date.Zone.GMT now)

let subject =
  let open Unstructured in
  [ v "Release"; sp 1; v "of"; sp 1; v "Mr."; sp 1; v "MIME" ]

let dkim_signature =
  let open Unstructured in
  [ v "v=1;"; sp 1
  ; v "a=rsa-sha256;"; sp 1
  ; v "c=relaxed/relaxed;"; sp 1
  ; v "d=github.com;"; sp 1
  ; v "s=pf2014;"; sp 1
  ; v "t=1544100336;"; sp 1
  ; v "bh=JC5Ppi0bIF0LjaqLBvsdmlGUddy9IBANMcZlB4r/jg0=;"; sp 1
  ; v "h=Date:From:Reply-To:To:Cc:In-Reply-To:References:\
       Subject:List-ID:List-Archive:List-Post:List-Unsubscribe:From;"; sp 1
  ; v "b=1crXUDuJO2iqzhbE/SM4v5F8MWzU2JaZmn8qvPPy0YzEfR9i\
       XTGe9maYVC0owrHzAHH6TYsRMvWFi4PIWQ4w9mf9n3w6hsMYqy\
       n6rTZwU02ObRNgZWgmWcrMwmDRdknvR/4EQZZOCdBtpE5Y95xJ\
       3x6xnS5k2QwT6OXovA7pczQ=" ]

let content = Content.make ~encoding:`Quoted_printable Content_type.default

let header =
  let open Header in
  Field.(Sender $ romain_calascibetta)
  & Field.(Date $ date)
  & Field.(To $ Address.[ group cambridge; mailbox yaron_minsky ])
  & Field.(Subject $ subject)
  & Field.(Field (Field_name.v "DKIM-Signature") $ dkim_signature)
  & Field.(Content $ content)
  & empty

let () =
  Hxd_string.pp Hxd.O.default Fmt.stdout (Header.to_string header)
