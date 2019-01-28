let valid_addresses =
  [ "Mary Smith <mary@example.net>"
  ; "Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>"
  ; "A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;"
  ; "Undisclosed recipients:;"
  ; "\"Mary Smith: Personal Account\" <smith@home.example>"
  ; "John Doe <jdoe@machine.example>"
  ; "Pete(A nice \\) chap) <pete(his account)@silly.test(his host)>"
  ; "A Group(Some people)\r\n\
    \    :Chris Jones <c@(Chris's host.)public.example>,\r\n\
    \      joe@example.org,\r\n\
    \  John <jdoe@one.test> (my dear friend); (the end of the group)"
  ; "(Empty list)(start)Hidden recipients  :(nobody(that I know))  ;"
  ; "Mary Smith <@node.test:mary@example.net>, , jdoe@test  . example"
  ; "Joe Q. Public <john.q.public@example.com>"
  ; "John Doe <jdoe@machine(comment).  example>"
  ; "Mary Smith\r\n  \r\n  <mary@example.net>"
  ; "<boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>"
  ; "!#$%&`*+/=?^`{|}~@iana.org"; "(\x07;)mary@example.net"
  ; "\"\\\x0a\"@x.test"; "\"\\a\"@x.test"; "\"\x07\"@x.test"
  ; "\"\\\x07\"@x.test"; "pete@[255.255.255.255]"; "\"mary\"@example.net"
  ; "\"\\\"\"@example.net"; "\"john\".\"public\"@example.com"
  ; "\"mary smith\"@home.example"; "\"mary\".smith@home.example"
  ; "\"mary\\\000\"@home.example"; " richard @home.example"
  ; "richar@ home .example"; "mary . smith@y.test"; "\x0d\x0a jdoe@example.net"
  ; " \x0d\x0a \x0d\x0a jdoe@example.net"; "(comment)smith@home.example"
  ; "(comment(comment))smith@home.example"; "smith@(comment)home.example"
  ; "smith@(comment)[255.255.255.255]"
  ; "robert@xn--hxajbheg2az3al.xn--jxalpdlp"; "xn--robert@x.test"
  ; "stephane+blog@bortzmeyer.org"; "{tropdur}@example.org"; "c&a@hotmail.com"
  ; "directeur@arts-premiers.museum"; "\"Stephane[Bortzmeyer]\"@laposte.net"
  ; "first.last@iana.org"
  ; "1234567890123456789012345678901234567890123456789012345678901234@iana.org"
  ; "\"first\\\"last\"@iana.org"; "\"first@last\"@iana.org"
  ; "\"first\\last\"@iana.org"; "first.last@[12.34.56.78]"
  ; "first.last@[IPv6:::12.34.56.78]"
  ; "first.last@[IPv6:1111:2222:3333::4444:12.34.56.78]"
  ; "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.56.78]"
  ; "first.last@[IPv6:::1111:2222:3333:4444:5555:6666]"
  ; "first.last@[IPv6:1111:2222:3333::4444:5555:6666]"
  ; "first.last@[IPv6:1111:2222:3333:4444:5555:6666::]"
  ; "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]"
  ; "first.last@x23456789012345678901234567890123456789012345678901234567890123.iana.org"
  ; "first.last@3com.com"; "first.last@123.iana.org"
  ; "first.last@[IPv6:1111:2222:3333::4444:5555:12.34.56.78]"
  ; "first.last@[IPv6:1111:2222:3333::4444:5555:6666:7777]"
  ; "first.last@example.123"; "first.last@com"; "\"Abc\\@def\"@iana.org"
  ; "\"Fred\\ Bloggs\"@iana.org"; "\"Joe.\\Blow\"@iana.org"
  ; "\"Abc@def\"@iana.org"; "\"Fred Bloggs\"@iana.org"; "user+mailbox@iana.org"
  ; "customer/department=shipping@iana.org"; "$A12345@iana.org"
  ; "!def!xyz%abc@iana.org"; "_somename@iana.org"; "dclo@us.ibm.com"
  ; "peter.piper@iana.org"; "\"Doug \\\"Ace\\\" L.\"@iana.org"; "test@iana.org"
  ; "TEST@iana.org"; "1234567890@iana.org"; "test+test@iana.org"
  ; "test-test@iana.org"; "t*est@iana.org"; "+1~1+@iana.org"
  ; "{_test_}@iana.org"; "\"[[ test  ]]\"@iana.org"; "test.test@iana.org"
  ; "\"test.test\"@iana.org"; "test.\"test\"@iana.org"
  ; "\"test@test\"@iana.org"; "test@123.123.123.x123"; "test@123.123.123.123"
  ; "test@[123.123.123.123]"; "test@example.iana.org"
  ; "test@example.example.iana.org"; "\"test\\test\"@iana.org"; "test@example"
  ; "\"test\\blah\"@iana.org"; "\"test\\\"blah\"@iana.org"
  ; "customer/department@iana.org"; "_Yosemite.Sam@iana.org"; "~@iana.org"
  ; "\"Austin@Powers\"@iana.org"; "Ima.Fool@iana.org"; "\"Ima.Fool\"@iana.org"
  ; "\"Ima Fool\"@iana.org"; "\"first\".\"last\"@iana.org"
  ; "\"first\".middle.\"last\"@iana.org"; "\"first\".last@iana.org"
  ; "first.\"last\"@iana.org"; "\"first\".\"middle\".\"last\"@iana.org"
  ; "\"first.middle\".\"last\"@iana.org"; "\"first.middle.last\"@iana.org"
  ; "\"first..last\"@iana.org"; "\"first\\\\\\\"last\"@iana.org"
  ; "first.\"mid\\dle\".\"last\"@iana.org"; "\"test blah\"@iana.org"
  ; "(foo)cal(bar)@(baz)iamcal.com(quux)"; "cal@iamcal(woo).(yay)com"
  ; "cal(woo(yay)hoopla)@iamcal.com"; "cal(foo\\@bar)@iamcal.com"
  ; "cal(foo\\)bar)@iamcal.com"; "first().last@iana.org"
  ; "pete(his account)@silly.test(his host)"; "c@(Chris's host.)public.example"
  ; "jdoe@machine(comment). example"; "1234 @ local(blah) .machine .example"
  ; "first(abc.def).last@iana.org"; "first(a\"bc.def).last@iana.org"
  ; "first.(\")middle.last(\")@iana.org"; "first(abc\\(def)@iana.org"
  ; "first.last@x(1234567890123456789012345678901234567890123456789012345678901234567890).com"
  ; "a(a(b(c)d(e(f))g)h(i)j)@iana.org"; "name.lastname@domain.com"; "a@b"
  ; "a@bar.com"; "aaa@[123.123.123.123]"; "a@bar"; "a-b@bar.com"; "+@b.c"
  ; "+@b.com"; "a@b.co-foo.uk"; "\"hello my name is\"@stutter.com"
  ; "\"Test \\\"Fail\\\" Ing\"@iana.org"; "valid@about.museum"
  ; "shaitan@my-domain.thisisminekthx"; "foobar@192.168.0.1"
  ; "\"Joe\\Blow\"@iana.org"
  ; "HM2Kinsists@(that comments are allowed)this.is.ok"
  ; "user%uucp!path@berkeley.edu"; "first.last @iana.org"
  ; "cdburgess+!#$%&'*-/=?+_{}|~test@gmail.com"
  ; "first.last@[IPv6:a1:a2:a3:a4:b1:b2:b3::]"; "first.last@[IPv6:::]"
  ; "first.last@[IPv6:::b4]"; "first.last@[IPv6:::b3:b4]"
  ; "first.last@[IPv6:a1::b4]"; "first.last@[IPv6:a1::]"
  ; "first.last@[IPv6:a1:a2::]"; "first.last@[IPv6:0123:4567:89ab:cdef::]"
  ; "first.last@[IPv6:0123:4567:89ab:CDEF::]"
  ; "first.last@[IPv6:::a3:a4:b1:ffff:11.22.33.44]"
  ; "first.last@[IPv6:a1:a2:a3:a4::11.22.33.44]"
  ; "first.last@[IPv6:a1:a2:a3:a4:b1::11.22.33.44]"
  ; "first.last@[IPv6:a1::11.22.33.44]"; "first.last@[IPv6:a1:a2::11.22.33.44]"
  ; "first.last@[IPv6:0123:4567:89ab:cdef::11.22.33.44]"
  ; "first.last@[IPv6:0123:4567:89ab:CDEF::11.22.33.44]"
  ; "first.last@[IPv6:a1::b2:11.22.33.44]"
  ; "first.last@[IPv6:::a2:a3:a4:b1:b2:b3:b4]"
  ; "first.last@[IPv6:::a2:a3:a4:b1:ffff:11.22.33.44]"; "test@test.com"
  ; "test@xn--example.com"; "test@example.com" ]

let invalid_addresses =
  [ ""; "mary"; "@"; "mary@"; "@io"; "@example.net"; ".mary@example.net"
  ; "jdoe.@example.net"; "pete..silly.test"; "sm_i-th.com"
  ; "mary\\@jdoe@one.test"; "jdoe@.one.test"; "jdon@one.test."
  ; "boss@nil..test"; "\"\"\"@example.net"; "\"\\\"@example.net"
  ; "jdoe\"@machine.example"; "\"jdoe@machine.example"
  ; "\"john\"public@example.com"; "john\"public\"@example.com"
  ; "\"john\"\"public\"@example.com"; "\"mary\000\"@home.example"
  ; "pete@a[255.255.255.255]"; "((comment)smith@home.example"
  ; "smith(coment)doe@home.example"; "robert@henry.com\r"
  ; "(smith@home.example"; "robert@[1.2.3.4"; "\"john\\\"@example.com"
  ; "(comment\\)smith@home.example"; "smith@home.example(comment\\)"
  ; "smith@home.example(comment\\"; "robert@[RFC5322-[domain-literal\\]"
  ; "robert@[RFC5322-[domain-literal]"; "robert@[RFC5322-[domain-literal\\"
  ; "marx@capitalism.ru\x0d"; "\x0dmarx@capitalism.ru"
  ; "\"\x0dmarx\"@capitalism.ru"; "(\x0d)marx@capitalism.ru"
  ; "marx@capitalism.ru(\x0d)"; "smith@communism.uk\x0a"
  ; "\x0asmith@communism.uk"; "\"\x0asmith\"@communism.uk"
  ; "(\x0a)smith@communism.uk"; "smith@communism.uk(\x0a)"
  ; "first.last@sub.do,com"; "first\\@last@iana.org"; "first.last"
  ; ".first.last@iana.org"; "first.last.@iana.org"; "first..last@iana.org"
  ; "\"first\"last\"@iana.org"; "\"\"\"@iana.org"; "\"\\\"@iana.org"
  ; "\"\"@iana.org"; "first\\@last@iana.org"; "first.last@"
  ; "first.last@[.12.34.56.78]"; "first.last@[12.34.56.789]"
  ; "first.last@[::12.34.56.78]"; "abc\\@def@iana.org"
  ; "abc\\@iana.org"; "@iana.org"; "doug@"; "\"qu@iana.org"; "ote\"@iana.org"
  ; ".dot@iana.org"; "dot.@iana.org"; "two..dot@iana.org"
  ; "\"Doug \"Ace\" L.\"@iana.org"; "Doug\\ \\\"Ace\\\"\\ L\\.@iana.org"
  ; "hello world@iana.org"; "gatsby@f.sc.ot.t.f.i.tzg.era.l.d."
  ; "test.iana.org"; "test.@iana.org"; "test..test@iana.org"; ".test@iana.org"
  ; "test@test@iana.org"; "test@@iana.org"; "-- test --@iana.org"
  ; "[test]@iana.org"; "\"test\"test\"@iana.org"; "()[]\\;:,><@iana.org"
  ; "test@."; "test@example."; "test@.org"; "test@[123.123.123.123"
  ; "test@123.123.123.123]"; "NotAnEmail"; "NotAnEmail"
  ; "\"test\"blah\"@iana.org"; ".wooly@iana.org"; "wo..oly@iana.org"
  ; "pootietang.@iana.org"; ".@iana.org"; "Ima Fool@iana.org"
  ; "phil.h\\@\\@ck@haacked.com"; "first\\last@iana.org"; "Abc\\@def@iana.org"
  ; "Fred\\ Bloggs@iana.org"; "Joe.\\Blow@iana.org"
  ; "{^c\\@**Dog^}@cartoon.com"; "cal(foo(bar)@iamcal.com"
  ; "cal(foo\\)@iamcal.com"; "cal(foo)bar)@iamcal.com"
  ; "first(middle)last@iana.org"; "a(a(b(c)d(e(f))g)(h(i)j)@iana.org"; ".@"
  ; "@bar.com"; "@@bar.com"; "aaa.com"; "aaa@.com"; "aaa@.com"; "aaa@.123"
  ; "aaa@[123.123.123.123]a"; "aaa@[123.123.123.333]"; "a@bar.com."; "-@..com"
  ; "-@a..com"; "test@...........com"; "\"\000 \"@char.com"; "\000@char.com"
  ; "=?us-ascii?Q?Chri's_Smith?= =?us-ascii?Q?Henry?= \
     <.@gmail.com,@hotmail.fr:henry.chris+porno@(Chris's \
     host.)public.example> (je suis un ******* en puissance)"
  ; "jdoe@[RFC-5322-\\a-domain-literal]"; "jdoe@[RFC-5322-\\t-domain-literal]"
  ; "jdoe@[RFC-5322-\\]-domain-literal]"
  ; "jdoe@[RFC-5322-domain-literal] (comment)" ]

(* XXX(dinosaure): below, addr are valid according RFC 5322 and RFC 5321 __ONLY
   IF__ returned result is a [general-address-literal]. However, domain values
   expect to be an IPv6 value. So [Ipaddr.V6.of_string] should fail here.

   So, if we parse these addresses and get a [general-address-literal] (see
   [Rfc5321.Ext]), they are invalid. *)

let invalid_addresses_according_rfc5321_without_general_address_literal =
  [ "first.last@[IPv6::]"; "first.last@[IPv6::::]"; "first.last@[IPv6::b4]"
  ; "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.567.89]"
  ; "first.last@[IPv6::::b4]"; "first.last@[IPv6::b3:b4]"
  ; "first.last@[IPv6::::b3:b4]"; "first.last@[IPv6:a1:::b4]"
  ; "first.last@[IPv6:a1:]"; "first.last@[IPv6:a1:::]"
  ; "first.last@[IPv6:a1:a2:]"; "first.last@[IPv6:a1:a2:::]"
  ; "first.last@[IPv6::11.22.33.44]"; "first.last@[IPv6::::11.22.33.44]"
  ; "first.last@[IPv6:a1:11.22.33.44]"; "first.last@[IPv6:a1:::11.22.33.44]"
  ; "first.last@[IPv6:a1:a2:::11.22.33.44]"
  ; "first.last@[IPv6:0123:4567:89ab:cdef::11.22.33.xx]"
  ; "first.last@[IPv6:0123:4567:89ab:CDEFF::11.22.33.44]"
  ; "first.last@[IPv6:a1::a4:b1::b4:11.22.33.44]"
  ; "first.last@[IPv6:a1::11.22.33]"; "first.last@[IPv6:a1::11.22.33.44.55]"
  ; "first.last@[IPv6:a1::b211.22.33.44]"; "first.last@[IPv6:a1::11.22.33]"
  ; "first.last@[IPv6:a1::11.22.33.44.55]"
  ; "first.last@[IPv6:a1::b211.22.33.44]"
  ; "first.last@[IPv6:a1::b2::11.22.33.44]"; "first.last@[IPv6:a1::b3:]"
  ; "first.last@[IPv6::a2::b4]"; "first.last@[IPv6:a1:a2:a3:a4:b1:b2:b3:]"
  ; "first.last@[IPv6::a2:a3:a4:b1:b2:b3:b4]"
  ; "first.last@[IPv6:a1:a2:a3:a4::b1:b2:b3:b4]"
  ; "first.last@[IPv5:::12.34.56.78]"
  ; "first.last@[IPv6:1111:2222:3333:4444:5555:12.34.56.78]"
  ; "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:12.34.56.78]"
  ; "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777]"
  ; "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888:9999]"
  ; "first.last@[IPv6:1111:2222::3333::4444:5555:6666]"
  ; "first.last@[IPv6:1111:2222:333x::4444:5555]"
  ; "first.last@[IPv6:1111:2222:33333::4444:5555]"; ]

let header_tests =
  [
(* See RFC 5322 § Appendix A.1.1 *)
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
  ]

exception Invalid_address
exception Invalid_header

let parse_address x =
  let x = x ^ "\r\n" in
  match
    Angstrom.(parse_string Mrmime.(Rfc5322.address_list <* Rfc822.crlf) x)
  with
  | Ok _ -> ()
  | Error _ -> raise Invalid_address

let parse_header x =
  let x = x ^ "\r\n" in
  match
    Angstrom.(parse_string Mrmime.(Mail.header <* Rfc822.crlf) x)
  with
  | Ok _ -> ()
  | Error _ -> raise Invalid_header

let valid_tests =
  List.map
    (fun input ->
      Alcotest.test_case input `Quick
      @@ fun () -> Alcotest.(check pass) input (parse_address input) () )
    valid_addresses

let invalid_tests =
  List.map
    (fun input ->
      Alcotest.test_case input `Quick
      @@ fun () ->
      Alcotest.check_raises input Invalid_address (fun () ->
          parse_address input ) )
    invalid_addresses

let failf fmt = Fmt.kstrf Alcotest.fail fmt

let domain_is_extension address =
  let domain_is_extension = function
    | `Addr (Mrmime.Rfc5321.Ext _) -> true
    | _ -> false in
  match address with
  | `Mailbox { Mrmime.Rfc5322.domain = head, tail; _ } ->
    List.for_all domain_is_extension (head :: tail)
  | `Group { Mrmime.Rfc5322.mailboxes; _ } ->
    let domains =
      List.fold_left
        (fun acc { Mrmime.Rfc5322.domain = head, tail; _ } -> (head :: tail) @ acc)
        [] mailboxes in
    List.for_all domain_is_extension domains

let invalid_tests_ipv6 =
  List.map
    (fun input ->
      Alcotest.test_case input `Quick
      @@ fun () -> match Angstrom.(parse_string Mrmime.(Rfc5322.address_list <* Rfc822.crlf)) (input ^ "\r\n") with
      | Ok addresses ->
        if not (List.for_all domain_is_extension addresses)
        then failf "Got something else than a general-address-literal: %s" input
      | Error _ -> () )
    invalid_addresses_according_rfc5321_without_general_address_literal

let header_tests =
  List.mapi
    (fun idx input ->
       Alcotest.test_case (Fmt.strf "header-%d" idx) `Quick
       @@ fun () -> Alcotest.(check pass) input (parse_header input) ())
    header_tests

let () =
  Alcotest.run "rfc5322" [ ("valid", valid_tests)
                         ; ("invalid", invalid_tests)
                         ; ("invalid", invalid_tests_ipv6)
                         ; ("header", header_tests) ]
