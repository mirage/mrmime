let header_tests =
  [ (* See RFC 5322 § Appendix A.1.1 *)
    {|From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.machine.example>
|};
    (* See RFC 5322 § Appendix A.1.2 *)
    {|From: "Joe Q. Public" <john.q.public@example.com>
To: Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>
Cc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net>
Date: Tue, 1 Jul 2003 10:52:37 +0200
Message-ID: <5678.21-Nov-1997@example.com>
|};
    (* See RFC 5322 § Appendix A.1.3 *)
    {|From: Pete <pete@silly.example>
To: A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;
Cc: Undisclosed recipients:;
Date: Thu, 13 Feb 1969 23:32:54 -0330
Message-ID: <testabcd.1234@silly.example>
|};
    (* See RFC 5322 § Appendix A.2 *)
    {|From: Mary Smith <mary@example.net>
To: John Doe <jdoe@machine.example>
Reply-To: "Mary Smith: Personal Account" <smith@home.example>
Subject: Re: Saying Hello
Date: Fri, 21 Nov 1997 10:01:10 -0600
Message-ID: <3456@example.net>
In-Reply-To: <1234@local.machine.example>
References: <1234@local.machine.example>
|};
    (* See RFC 5322 § Appendix A.3 *)
    {|Resent-From: Mary Smith <mary@example.net>
Resent-To: Jane Brown <j-brown@other.example>
Resent-Date: Mon, 24 Nov 1997 14:22:01 -0800
Resent-Message-ID: <78910@example.net>
From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.machine.example>
|};
    (* See RFC 5322 § Appendix A.4 *)
    {|Received: from x.y.test
   by example.net
   via TCP
   with ESMTP
   id ABC12345
   for <mary@example.net>;  21 Nov 1997 10:05:43 -0600
Received: from node.example by x.y.test; 21 Nov 1997 10:01:22 -0600
From: John Doe <jdoe@node.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.node.example>
|};
    (* See RFC 5322 § Appendix A.5 *)
    {|From: Pete(A nice \) chap) <pete(his account)@silly.test(his host)>
To:A Group(Some people)
     :Chris Jones <c@(Chris's host.)public.example>,
         joe@example.org,
  John <jdoe@one.test> (my dear friend); (the end of the group)
Cc:(Empty list)(start)Hidden recipients  :(nobody(that I know))  ;
Date: Thu,
      13
        Feb
          1969
      23:32
               -0330 (Newfoundland Time)
Message-ID:              <testabcd.1234@silly.test>
|};
    (* See RFC 5322 § Appendix A.6.1 *)
    {|From: Joe Q. Public <john.q.public@example.com>
To: Mary Smith <@node.test:mary@example.net>, , jdoe@test  . example
Date: Tue, 1 Jul 2003 10:52:37 +0200
Message-ID: <5678.21-Nov-1997@example.com>
|};
    (* See RFC 5322 § Appendix A.6.2 *)
    {|From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: 21 Nov 97 09:55:06 GMT
Message-ID: <1234@local.machine.example>
|};
    (* See RFC 5322 § Appendix A.6.3 *)
    {|From  : John Doe <jdoe@machine(comment).  example>
To    : Mary Smith
  
          <mary@example.net>
Subject     : Saying Hello
Date  : Fri, 21 Nov 1997 09(comment):   55  :  06 -0600
Message-ID  : <1234   @   local(blah)  .machine .example>
|};
    (* See RFC 822 § A.3.1 *)
    {|Date:     26 Aug 76 14:29 EDT
From:     Jones@Registry.Org
Bcc:
|};
    (* See RFC 822 § A.3.2 *)
    {|Date:     26 Aug 76 14:30 EDT
From:     George Jones<Group@Host>
Sender:   Secy@SHOST
To:       "Al Neuman"@Mad-Host,
          Sam.Irving@Other-Host
Message-ID:  <some.string@SHOST>
|};
    (* See RFC 822 § A.3.3 *)
    {|Date     :  27 Aug 76 09:32 PDT
From     :  Ken Davis <KDavis@This-Host.This-net>
Subject  :  Re: The Syntax in the RFC
Sender   :  KSecy@Other-Host
Reply-To :  Sam.Irving@Reg.Organization
To       :  George Jones <Group@Some-Reg.An-Org>,
            Al.Neuman@MAD.Publisher
cc       :  Important folk:
              Tom Softwood <Balsa@Tree.Root>,
              "Sam Irving"@Other-Host;,
            Standard Distribution:
              /main/davis/people/standard@Other-Host,
              "<Jones>standard.dist.3"@Tops-20-Host>;
Comment  : Sam is away on business. He asked me to handle
           his mail for him.  He'll be able to provide  a
           more  accurate  explanation  when  he  returns
           next week.
In-Reply-To: <some.string@DBM.Group>, George's message
X-Special-action:  This is a sample of user-defined field-
            names.  There could also be a field-name
            "Special-action", but its name might later be
            preempted
Message-ID: <4231.629.XYzi-What@Other-Host>
|};
    (* See RFC 2047 § 8 *)
    {|From: =?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>
Date     :  27 Aug 76 09:32 PDT
To: =?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>
CC: =?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>
Subject: =?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?=
 =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=
|};
    (* See RFC 2047 § 8 *)
    {|From: =?ISO-8859-1?Q?Olle_J=E4rnefors?= <ojarnef@admin.kth.se>
To: ietf-822@dimacs.rutgers.edu, ojarnef@admin.kth.se
Subject: Time for ISO 10646?
|};
    (* See RFC 2047 § 8 *)
    {|To: Dave Crocker <dcrocker@mordor.stanford.edu>
Cc: ietf-822@dimacs.rutgers.edu, paf@comsol.se
From: =?ISO-8859-1?Q?Patrik_F=E4ltstr=F6m?= <paf@nada.kth.se>
Subject: Re: RFC-HDR care and feeding
|};
    (* See RFC 2047 § 8 *)
    {|From: Nathaniel Borenstein <nsb@thumper.bellcore.com>
      (=?iso-8859-8?b?7eXs+SDv4SDp7Oj08A==?=)
To: Greg Vaudreuil <gvaudre@NRI.Reston.VA.US>, Ned Freed
   <ned@innosoft.com>, Keith Moore <moore@cs.utk.edu>
Subject: Test of new header generator
MIME-Version: 1.0
Content-type: text/plain; charset=ISO-8859-1
|}
  ]

let parse_header x =
  match
    Angstrom.parse_string ~consume:Angstrom.Consume.Prefix
      (Mrmime.Header.Decoder.header None)
      (x ^ "\r\n")
  with
  | Ok header -> header
  | Error _ -> Fmt.failwith "Invalid header"

let parse_and_print_header x =
  parse_header x |> Fmt.pr "header: @[<hov>%a@].\n%!" Mrmime.Header.pp

let header_tests =
  let make idx input =
    Alcotest.test_case (Fmt.str "header %d" idx) `Quick @@ fun () ->
    Alcotest.(check pass) input (parse_and_print_header input) ()
  in
  List.mapi make header_tests

(* [parse_content_type x expected] extracts a content-type header from
   x and compares its types, subtypes and parameters counts to the
   expected values [expected] *)
let parse_content_type x (ty, subty, param_count) =
  let open Mrmime in
  let content_type = parse_header x |> Header.assoc Field_name.content_type in
  let ty', subty', param_count' =
    match content_type with
    | [ Mrmime.Field.Field (_, w, v) ] -> (
        match w with
        | Field.Content ->
            ( Content_type.ty v,
              Content_type.subty v,
              Content_type.parameters v |> List.length )
        | _ -> Fmt.failwith "Not a content type.")
    | _ -> Fmt.failwith "Invalid header"
  in
  if not (Content_type.Type.equal ty ty') then
    Fmt.failwith "Content-type don't matched"
  else if not (Content_type.Subtype.equal subty subty') then
    Fmt.failwith "Content-subtype don't matched"
  else if not (param_count = param_count') then
    Fmt.failwith "Not the same number of parameters."
  else ()

(* Check that whitespaces are allowed in content type parameter value (PR#72) *)
let content_type_test =
  let test =
    {|From: Nathaniel Borenstein <nsb@thumper.bellcore.com>|}^"\r"^{|
      (=?iso-8859-8?b?7eXs+SDv4SDp7Oj08A==?=)|}^"\r"^{|
To: Greg Vaudreuil <gvaudre@NRI.Reston.VA.US>, Ned Freed|}^"\r"^{|
   <ned@innosoft.com>, Keith Moore <moore@cs.utk.edu>|}^"\r"^{|
Subject: Test of new header generator|}^"\r"^{|
MIME-Version: 1.0|}^"\r"^{|
Content-type: text/plain; wpefjjnqwisj231=" q02eifwe0sn  "; weinfw="qwewqe"|}^"\r"^{|
|}
  in
  let ct =
    Mrmime.Content_type.(Type.text, Subtype.iana_exn Type.text "plain", 2)
  in
  Alcotest.test_case "header - content-type" `Quick @@ fun () ->
  Alcotest.(check pass) test (parse_content_type test ct) ()

let () =
  Alcotest.run "rfc5322" [ ("header", header_tests @ [ content_type_test ]) ]
