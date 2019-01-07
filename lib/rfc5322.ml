type phrase =
  [`Dot | `Word of Rfc822.word | `Encoded of Rfc2047.encoded_word] list

type domain =
  [ `Domain of string list
  | `Literal of string
  | `Addr of Rfc5321.literal_domain ]

type mailbox =
  {name: phrase option; local: Rfc822.local; domain: domain * domain list}

(* / *)

let pp_word ppf = function
  | `Atom x -> Fmt.string ppf x
  | `String x -> Fmt.pf ppf "%S" x

let pp_phrase : phrase Fmt.t =
  let pp_elt ppf = function
    | `Dot -> Fmt.string ppf "."
    | `Word w -> pp_word ppf w
    | `Encoded e -> Encoded_word.pp ppf e
  in
  Fmt.list ~sep:Fmt.(fun ppf () -> fmt "@ " ppf) pp_elt

let pp_literal_domain ppf = function
  | Rfc5321.IPv4 v -> Ipaddr.V4.pp ppf v
  | Rfc5321.IPv6 v -> Ipaddr.V6.pp ppf v
  | Rfc5321.Ext (ldh, value) -> Fmt.pf ppf "%s:%s" ldh value

let pp_domain ppf = function
  | `Domain l -> Fmt.list ~sep:Fmt.(const string ".") Fmt.string ppf l
  | `Literal s -> Fmt.pf ppf "[%s]" s
  | `Addr x -> Fmt.pf ppf "[%a]" pp_literal_domain x

let pp_local = Fmt.list ~sep:Fmt.(const string ".") pp_word

let pp_mailbox =
 fun ppf x ->
  Fmt.pf ppf "{ @[<hov>name = %a;@ local = %a;@ domain = %a@] }"
    Fmt.(hvbox (Dump.option pp_phrase))
    x.name
    Fmt.(hvbox pp_local)
    x.local
    Fmt.(hvbox (Dump.pair pp_domain (Dump.list pp_domain)))
    x.domain

(* / *)

type group = {name: phrase; mailboxes: mailbox list}
type address = [`Group of group | `Mailbox of mailbox]

type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

type zone =
  | UT
  | GMT
  | EST
  | EDT
  | CST
  | CDT
  | MST
  | MDT
  | PST
  | PDT
  | Military_zone of char
  | TZ of int * int

type date =
  { day: day option
  ; date: int * month * int
  ; time: int * int * int option
  ; zone: zone }

type unstructured =
  [ `Text of string
  | `CR of int
  | `LF of int
  | `CRLF
  | `WSP
  | `Encoded of Rfc2047.encoded_word ]
  list

type phrase_or_msg_id = [`Phrase of phrase | `MsgID of Rfc822.nonsense Rfc822.msg_id]

type resent =
  [ `ResentDate of date
  | `ResentFrom of mailbox list
  | `ResentSender of mailbox
  | `ResentTo of address list
  | `ResentCc of address list
  | `ResentBcc of address list
  | `ResentMessageID of Rfc822.nonsense Rfc822.msg_id
  | `ResentReplyTo of address list ]

type trace =
  [ `Trace of
    mailbox option
    * ( [`Addr of mailbox | `Domain of domain | `Word of Rfc822.word] list
      * date option )
      list ]

type field_header =
  [ `Date of date
  | `From of mailbox list
  | `Sender of mailbox
  | `ReplyTo of address list
  | `To of address list
  | `Cc of address list
  | `Bcc of address list
  | `MessageID of Rfc822.nonsense Rfc822.msg_id
  | `InReplyTo of phrase_or_msg_id list
  | `References of phrase_or_msg_id list
  | `Subject of unstructured
  | `Comments of unstructured
  | `Keywords of phrase list
  | `Field of string * unstructured
  | `Unsafe of string * unstructured ]

type skip = [`Skip of string list]
type field = [field_header | resent | trace | skip]

open Angstrom

(* From RFC 822

     domain-literal =  "[" *(dtext / quoted-pair) "]"

     o   Square brackets ("[" and "]") are used to indicate the
         presence  of  a  domain-literal, which the appropriate
         name-domain  is  to  use  directly,  bypassing  normal
         name-resolution mechanisms.

     Domain-literals which refer to domains within the ARPA  Inter-
     net  specify  32-bit  Internet addresses, in four 8-bit fields
     noted in decimal, as described in Request for  Comments  #820,
     "Assigned Numbers."  For example:

                              [10.0.3.19]

     Note:  THE USE OF DOMAIN-LITERALS IS STRONGLY DISCOURAGED.  It
            is  permitted  only  as  a means of bypassing temporary
            system limitations, such as name tables which  are  not
            complete.

   From RFC 2822

     domain-literal  =       [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]

   From RFC 5322

     domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]

   XXX(dinosaure): semantically the same as [Rfc822.domain_literal]
*)
let domain_literal =
  option () Rfc822.cfws
  *> char '['
  *> ( many
         ( option (false, false, false) Rfc822.fws
         *> ( Rfc6532.with_uutf1 Rfc822.is_dtext
            <|> (Rfc822.quoted_pair >>| String.make 1) ) )
     >>| String.concat "" )
  <* option (false, false, false) Rfc822.fws
  <* char ']'
  <* option () Rfc822.cfws

let domain = Rfc5321.domain

(* From RFC 724

     <host-indicator>  ::=   <at-indicator> <host-name>
     <host-name>       ::=   <atom>  | <decimal host address>
     <host-phrase>     ::=   <phrase> <host-indicator>

   From RFC 822

     addr-spec   =  local-part "@" domain        ; global address

   From RFC 2822

     An addr-spec is a specific Internet identifier that contains a
     locally interpreted string followed by the at-sign character ("@",
     ASCII value 64) followed by an Internet domain.  The locally
     interpreted string is either a quoted-string or a dot-atom.  If the
     string can be represented as a dot-atom (that is, it contains no
     characters other than atext characters or "." surrounded by atext
     characters), then the dot-atom form SHOULD be used and the
     quoted-string form SHOULD NOT be used. Comments and folding white
     space SHOULD NOT be used around the "@" in the addr-spec.

     addr-spec       =       local-part "@" domain

   From RFC 5322

     Note: A liberal syntax for the domain portion of addr-spec is
     given here.  However, the domain portion contains addressing
     information specified by and used in other protocols (e.g.,
     [RFC1034], [RFC1035], [RFC1123], [RFC5321]).  It is therefore
     incumbent upon implementations to conform to the syntax of
     addresses for the context in which they are used.

     addr-spec       =   local-part "@" domain
*)
let addr_spec =
  lift2
    (fun local domain -> {name= None; local; domain= (domain, [])})
    Rfc822.local_part
    (char '@' *> domain)

(* From RFC 822

     word        =  atom / quoted-string

   From RFC 2047

     5. Use of encoded-words in message headers

        An 'encoded-word' may appear in a message header or body part header
        according to the following rules:

     (1) An 'encoded-word' may replace a 'text' token (as defined by RFC 822)
         in any Subject or Comments header field, any extension message
         header field, or any MIME body part field for which the field body
         is defined as '*text'.  An 'encoded-word' may also appear in any
         user-defined ("X-") message or body part header field.

         Ordinary ASCII text and 'encoded-word's may appear together in the
         same header field.  However, an 'encoded-word' that appears in a
         header field defined as '*text' MUST be separated from any adjacent
         'encoded-word' or 'text' by 'linear-white-space'.

     (2) An 'encoded-word' may appear within a 'comment' delimited by "(" and
         ")", i.e., wherever a 'ctext' is allowed.  More precisely, the RFC
         822 ABNF definition for 'comment' is amended as follows:

         comment = "(" *(ctext / quoted-pair / comment / encoded-word) ")"

         A "Q"-encoded 'encoded-word' which appears in a 'comment' MUST NOT
         contain the characters "(", ")" or DQUOTE
         'encoded-word' that appears in a 'comment' MUST be separated from
         any adjacent 'encoded-word' or 'ctext' by 'linear-white-space'.

         It is important to note that 'comment's are only recognized inside
         "structured" field bodies.  In fields whose bodies are defined as
         '*text', "(" and ")" are treated as ordinary characters rather than
         comment delimiters, and rule (1) of this section applies.  (See RFC
         822, sections 3.1.2 and 3.1.3)

     (3) As a replacement for a 'word' entity within a 'phrase', for example,
         one that precedes an address in a From, To, or Cc header.  The ABNF
         definition for 'phrase' from RFC 822 thus becomes:

         phrase = 1 *( encoded-word / word )

         In this case the set of characters that may be used in a "Q"-encoded
         'encoded-word' is restricted to: <upper and lower case ASCII
         letters, decimal digits, "!", "*", "+", "-", "/", "=", and "_"
         (underscore, ASCII 95.)>.  An 'encoded-word' that appears within a
         'phrase' MUST be separated from any adjacent 'word', 'text' or
         'special' by 'linear-white-space'.

        These are the ONLY locations where an 'encoded-word' may appear.  In
        particular:

        + An 'encoded-word' MUST NOT appear in any portion of an 'addr-spec'.

        + An 'encoded-word' MUST NOT appear within a 'quoted-string'.

        + An 'encoded-word' MUST NOT be used in a Received header field.

        + An 'encoded-word' MUST NOT be used in parameter of a MIME
          Content-Type or Content-Disposition field, or in any structured
          field body except within a 'comment' or 'phrase'.

   From RFC 2822

     word            =       atom / quoted-string

   From RFC 5322

     word            =   atom / quoted-string
*)
let word_with_encoded_word =
  option () Rfc822.cfws *> (Rfc2047.encoded_word >>| fun x -> `Encoded x)
  <* option () Rfc822.cfws
  <|> (Rfc822.word >>| fun x -> `Word x)

(* From RFC 724

         <phrase>          ::=   <word>
                               | <word> <phrase>

   From RFC 822

     phrase      =  1*word                       ; Sequence of words

        The one exception to this rule  is  that  a  single  SPACE  is
        assumed  to  exist  between  contiguous words in a phrase, and
        this interpretation is independent of  the  actual  number  of
        LWSP-chars  that  the  creator  places  between the words.  To
        include more than one SPACE, the creator must make  the  LWSP-
        chars be part of a quoted-string.

   From RFC 2822

     obs-phrase      =       word *(word / "." / CFWS)

   From RFC 5322

        Note: The "period" (or "full stop") character (".") in obs-phrase
        is not a form that was allowed in earlier versions of this or any
        other specification.  Period (nor any other character from
        specials) was not allowed in phrase because it introduced a
        parsing difficulty distinguishing between phrases and portions of
        an addr-spec (see section 4.4).  It appears here because the
        period character is currently used in many messages in the
        display-name portion of addresses, especially for initials in
        names, and therefore must be interpreted properly.

     obs-phrase      =   word *(word / "." / CFWS)
*)
let obs_phrase =
  word_with_encoded_word
  >>= fun first ->
  fix (fun m ->
      lift2
        (function
          | (`Dot | `Word _ | `Encoded _) as x -> fun r -> x :: r
          | `CFWS -> fun r -> r)
        ( word_with_encoded_word
        <|> (char '.' >>| fun _ -> `Dot)
        <|> (Rfc822.cfws >>| fun () -> `CFWS) )
        m
      <|> return [] )
  >>| fun rest -> first :: rest

(* From RFC 724

     <phrase>          ::=   <word>
                           | <word> <phrase>

   From RFC 822

     phrase      =  1*word                       ; Sequence of words

        The one exception to this rule  is  that  a  single  SPACE  is
        assumed  to  exist  between  contiguous words in a phrase, and
        this interpretation is independent of  the  actual  number  of
        LWSP-chars  that  the  creator  places  between the words.  To
        include more than one SPACE, the creator must make  the  LWSP-
        chars be part of a quoted-string.

   From RFC 1342

     - As a replacement for a "word" entity within a "phrase", for example,
       one that precedes an address in a From, To, or Cc header.  The EBNF
       definition for phrase from RFC 822 thus becomes:

       phrase = 1*(encoded-word / word)

     XXX(dinosaure): RFC 1342 explains something more but implementation
     should be more explained in rfc2047.ml

   From RFC 1522

     (3) As a replacement for a "word" entity within a "phrase", for
         example, one that precedes an address in a From, To, or Cc
         header.  The ABNF definition for phrase from RFC 822 thus
         becomes:

         phrase = 1*(encoded-word / word)

         In this case the set of characters that may be used in a "Q"-
         encoded encoded-word is restricted to: <upper and lower case
         ASCII letters, decimal digits, "!", "*", "+", "-", "/", "=", and
         "_" (underscore, ASCII 95.)>.  An encoded-word that appears
         within a "phrase" MUST be separated from any adjacent "word",
         "text" or "special" by linear-white-space.

   From RFC 2047

     (3) As a replacement for a 'word' entity within a 'phrase', for example,
         one that precedes an address in a From, To, or Cc header.  The ABNF
         definition for 'phrase' from RFC 822 thus becomes:

         phrase = 1*( encoded-word / word )

         In this case the set of characters that may be used in a "Q"-encoded
         'encoded-word' is restricted to: <upper and lower case ASCII
         letters, decimal digits, "!", "*", "+", "-", "/", "=", and "_"
         (underscore, ASCII 95.)>.  An 'encoded-word' that appears within a
         'phrase' MUST be separated from any adjacent 'word', 'text' or
         'special' by 'linear-white-space'.

   From RFC 2822

     phrase          =       1*word / obs-phrase
     obs-phrase      =       word *(word / "." / CFWS)

     Differences from earlier standards:

     1. Period allowed in obsolete form of phrase.

   From RFC 5322

     phrase          =   1*word / obs-phrase

       Note: The "period" (or "full stop") character (".") in obs-phrase
       is not a form that was allowed in earlier versions of this or any
       other specification.  Period (nor any other character from
       specials) was not allowed in phrase because it introduced a
       parsing difficulty distinguishing between phrases and portions of
       an addr-spec (see section 4.4).  It appears here because the
       period character is currently used in many messages in the
       display-name portion of addresses, especially for initials in
       names, and therefore must be interpreted properly.

     obs-phrase      =   word *(word / "." / CFWS)

     Differences from Earlier Specifications

     1.   Period allowed in obsolete form of phrase.
*)
let phrase = obs_phrase <|> many1 word_with_encoded_word

(* From RFC 2822

     display-name    =       phrase

   From RFC 5322

     display-name    =   phrase
*)
let display_name = phrase

(* From RFC 2822

     obs-domain-list =       "@" domain *( *(CFWS / "," ) [CFWS] "@" domain)

   From RFC 5322

     obs-domain-list =   *(CFWS / ",") "@" domain
                         *("," [CFWS] ["@" domain])
*)
let obs_domain_list =
  let first =
    fix (fun m ->
        lift2 (fun _ _ -> ()) (Rfc822.cfws <|> (char ',' >>| fun _ -> ())) m
        <|> return () )
  in
  let rest =
    fix (fun m ->
        lift2
          (function `Sep -> fun r -> r | `Domain x -> fun r -> x :: r)
          ( char ','
          *> option () Rfc822.cfws
          *> option `Sep (char '@' *> domain >>| fun x -> `Domain x) )
          m
        <|> return [] )
  in
  first *> char '@' *> domain >>= fun x -> rest >>| fun r -> x :: r

(* From RFC 2822

     obs-route       =       [CFWS] obs-domain-list ":" [CFWS]

   From RFC 5322

     obs-route       =   obs-domain-list ":"
*)
let obs_route = obs_domain_list <* char ':'

(* From RFC 2822

     obs-angle-addr  =       [CFWS] "<" [obs-route] addr-spec ">" [CFWS]

   From RFC 5322

     obs-angle-addr  =   [CFWS] "<" obs-route addr-spec ">" [CFWS]
*)
let obs_angle_addr =
  option () Rfc822.cfws *> char '<' *> obs_route
  >>= fun domains ->
  addr_spec
  >>= fun {name; local; domain= domain, _; _} ->
  char '>' *> option () Rfc822.cfws
  >>| fun () -> {name; local; domain= (domain, domains)}

(* From RFC 2822

     angle-addr      =       [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr

   From RFC 5322

     angle-addr      =   [CFWS] "<" addr-spec ">" [CFWS] /
                         obs-angle-addr
*)
let angle_addr =
  obs_angle_addr
  <|> ( option () Rfc822.cfws *> char '<' *> addr_spec
      >>= fun {name; local; domain= domain, _; _} ->
      char '>' *> option () Rfc822.cfws
      >>| fun _ -> {name; local; domain= (domain, [])} )

(* From RFC 2822

     name-addr       =       [display-name] angle-addr

   From RFC 5322

     name-addr       =   [display-name] angle-addr
*)
let name_addr =
  option None (display_name >>| fun x -> Some x)
  >>= fun name -> angle_addr >>| fun {local; domain; _} -> {name; local; domain}

(* From RFC 724

     <mailbox>         ::=   <host-phrase>

   From RFC 822

     mailbox     =  addr-spec                    ; simple address
                 /  phrase route-addr            ; name & addr-spec

   From RFC 2822

     mailbox         =       name-addr / addr-spec

   From RFC 5322

     mailbox         =   name-addr / addr-spec
*)
let mailbox = name_addr <|> addr_spec

(* From RFC 2822

     obs-mbox-list   =       1*([mailbox] [CFWS] "," [CFWS]) [mailbox]

   From RFC 5322

     obs-mbox-list   =   *([CFWS] ",") mailbox *("," [mailbox / CFWS])

     The following are changes from [RFC2822]:
     17.  Fixed all obsolete list syntax (obs-domain-list, obs-mbox-list,
          obs-addr-list, obs-phrase-list, and the newly added obs-group-
          list).
*)
let obs_mbox_list =
  let rest =
    fix (fun m ->
        lift2
          (function `Mailbox x -> fun r -> x :: r | `Sep -> fun r -> r)
          ( char ','
          *> option `Sep
               ( mailbox
               >>| (fun m -> `Mailbox m)
               <|> (Rfc822.cfws >>| fun () -> `Sep) ) )
          m
        <|> return [] )
  in
  many (option () Rfc822.cfws *> char ',') *> mailbox
  >>= fun x -> rest >>| fun r -> x :: r

(* From RFC 5322

     obs-group-list  =   1*([CFWS] ",") [CFWS]
*)
let obs_group_list =
  many1 (option () Rfc822.cfws *> char ',') *> option () Rfc822.cfws

(* From RFC 724

     <mailbox-list>    ::=   <mailbox>
                           | <mailbox> "," <mailbox-list>

   From RFC 2822

     mailbox-list    =       (mailbox *("," mailbox)) / obs-mbox-list

   From RFC 5322

     mailbox-list    =   (mailbox *("," mailbox)) / obs-mbox-list
*)
let mailbox_list =
  obs_mbox_list
  <|> (mailbox >>= fun x -> many (char ',' *> mailbox) >>| fun r -> x :: r)

(* From RFC 5322

     group-list      =   mailbox-list / CFWS / obs-group-list
*)
let group_list =
  mailbox_list
  <|> (obs_group_list >>| fun () -> [])
  <|> (Rfc822.cfws >>| fun () -> [])

(* From RFC 2822

     group           =       display-name ":" [mailbox-list / CFWS] ";"
                             [CFWS]

   From RFC 5322

     group           =   display-name ":" [group-list] ";" [CFWS]
*)
let group =
  display_name
  >>= fun name ->
  char ':' *> option [] group_list
  >>= fun mailboxes ->
  char ';' *> option () Rfc822.cfws >>| fun _ -> {name; mailboxes}

(* From RFC 822

     address     =  mailbox                      ; one addressee
                 /  group                        ; named list

   From RFC 2822

     address         =       mailbox / group

   From RFC 5322

     address         =   mailbox / group
*)
let address =
  group >>| (fun g -> `Group g) <|> (mailbox >>| fun m -> `Mailbox m)

(* From RFC 2822

     obs-addr-list   =       1*([address] [CFWS] "," [CFWS]) [address]

   From RFC 5322

     obs-addr-list   =   *([CFWS] ",") address *("," [address / CFWS])
*)
let obs_addr_list =
  let rest =
    fix (fun m ->
        lift2
          (function `Addr x -> fun r -> x :: r | `Sep -> fun r -> r)
          ( char ','
          *> option `Sep
               ( address
               >>| (fun a -> `Addr a)
               <|> (Rfc822.cfws >>| fun () -> `Sep) ) )
          m
        <|> return [] )
  in
  many (option () Rfc822.cfws *> char ',') *> address
  >>= fun x -> rest >>| fun r -> x :: r

(* From RFC 724

     <address-list>    ::=   <null>
                           | <address-item>
                           | <address-item> "," <address-list>

   From RFC 2822

     address-list    =       (address *("," address)) / obs-addr-list

   From RFC 5322

     address-list    =   (address *("," address)) / obs-addr-list
*)
let address_list =
  obs_addr_list
  <|> (address >>= fun x -> many (char ',' *> address) >>| fun r -> x :: r)

let is_digit = function '0' .. '9' -> true | _ -> false

let two_digit =
  lift2
    (fun a b ->
      let res = Bytes.create 2 in
      Bytes.unsafe_set res 0 a ;
      Bytes.unsafe_set res 1 b ;
      Bytes.unsafe_to_string res )
    (satisfy is_digit) (satisfy is_digit)

let four_digit =
  lift4
    (fun a b c d ->
      let res = Bytes.create 4 in
      Bytes.unsafe_set res 0 a ;
      Bytes.unsafe_set res 1 b ;
      Bytes.unsafe_set res 2 c ;
      Bytes.unsafe_set res 3 d ;
      Bytes.unsafe_to_string res )
    (satisfy is_digit) (satisfy is_digit) (satisfy is_digit) (satisfy is_digit)

let at_least_n_digit n =
  take_while1 is_digit
  >>= fun res ->
  if String.length res >= n then return res else fail "at_least_n_digit"

let one_or_two_digit =
  satisfy is_digit
  >>= fun one ->
  peek_char
  >>= function
  | Some two when is_digit two ->
      let res = Bytes.create 2 in
      Bytes.unsafe_set res 0 one ;
      Bytes.unsafe_set res 1 two ;
      advance 1 *> return (Bytes.unsafe_to_string res)
  | _ -> return (String.make 1 one)

(* From RFC 2822

     obs-hour        =       [CFWS] 2DIGIT [CFWS]

   From RFC 5322

     obs-hour        =   [CFWS] 2DIGIT [CFWS]
*)
let obs_hour =
  option () Rfc822.cfws *> two_digit <* option () Rfc822.cfws >>| int_of_string

(* From RFC 2822

     obs-minute      =       [CFWS] 2DIGIT [CFWS]

   From RFC 5322

     obs-minute      =   [CFWS] 2DIGIT [CFWS]
*)
let obs_minute = option () Rfc822.cfws *> two_digit <* option () Rfc822.cfws >>| int_of_string

(* From RFC 2822

     obs-second      =       [CFWS] 2DIGIT [CFWS]

   From RFC 5322

     obs-second      =   [CFWS] 2DIGIT [CFWS]
*)
let obs_second = option () Rfc822.cfws *> two_digit <* option () Rfc822.cfws >>| int_of_string

(* From RFC 2822

     hour            =       2DIGIT / obs-hour

   From RFC 5322

     hour            =   2DIGIT / obs-hour
*)
let hour = obs_hour <|> (two_digit >>| int_of_string)

(* From RFC 2822

     minute          =       2DIGIT / obs-minute

   From RFC 5322

     minute          =   2DIGIT / obs-minute
*)
let minute = obs_minute <|> (two_digit >>| int_of_string)

(* From RFC 2822

     second          =       2DIGIT / obs-second

   From RFC 5322

     second          =   2DIGIT / obs-second
*)
let second = obs_second <|> (two_digit >>| int_of_string)

(* From RFC 2822

     obs-year        =       [CFWS] 2*DIGIT [CFWS]

   From RFC 5322

     obs-year        =   [CFWS] 2*DIGIT [CFWS]
*)
let obs_year =
  option () Rfc822.cfws *> at_least_n_digit 2
  <* option () Rfc822.cfws
  >>| int_of_string

(* From RFC 2822

     year            =       4*DIGIT / obs-year

     Where a two or three digit year occurs in a date, the year is to be
     interpreted as follows: If a two digit year is encountered whose
     value is between 00 and 49, the year is interpreted by adding 2000,
     ending up with a value between 2000 and 2049.  If a two digit year is
     encountered with a value between 50 and 99, or any three digit year
     is encountered, the year is interpreted by adding 1900.

     Differences from Earlier Specifications
     3.   Four or more digits allowed for year.
     15.  Two digit years not allowed.*
     16.  Three digit years interpreted, but not allowed for generation.*

   From RFC 5322

     year            =   (FWS 4*DIGIT FWS) / obs-year

     Where a two or three digit year occurs in a date, the year is to be
     interpreted as follows: If a two digit year is encountered whose
     value is between 00 and 49, the year is interpreted by adding 2000,
     ending up with a value between 2000 and 2049.  If a two digit year is
     encountered with a value between 50 and 99, or any three digit year
     is encountered, the year is interpreted by adding 1900.

     Differences from Earlier Specifications
     3.   Four or more digits allowed for year.
     15.  Two digit years not allowed.*
     16.  Three digit years interpreted, but not allowed for generation.*
*)
let year =
  Rfc822.fws *> at_least_n_digit 4 <* Rfc822.fws >>| int_of_string <|> obs_year

(* From RFC 2822

     obs-day         =       [CFWS] 1*2DIGIT [CFWS]

   From RFC 5322

     obs-day         =   [CFWS] 1*2DIGIT [CFWS]
*)
let obs_day =
  option () Rfc822.cfws *> one_or_two_digit
  <* option () Rfc822.cfws
  >>| int_of_string

(* From RFC 2822

     day             =       ([FWS] 1*2DIGIT) / obs-day

   From RFC 5322

     day             =   ([FWS] 1*2DIGIT FWS) / obs-day
*)
let day =
  obs_day
  <|> ( option (false, false, false) Rfc822.fws *> one_or_two_digit
      <* Rfc822.fws
      >>| int_of_string )

(* From RFC 822

     month       =  "Jan"  /  "Feb" /  "Mar"  /  "Apr"
                 /  "May"  /  "Jun" /  "Jul"  /  "Aug"
                 /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"

   From RFC 2822

     month           =       (FWS month-name FWS) / obs-month

     month-name      =       "Jan" / "Feb" / "Mar" / "Apr" /
                             "May" / "Jun" / "Jul" / "Aug" /
                             "Sep" / "Oct" / "Nov" / "Dec"

   From RFC 5322

     month           =   "Jan" / "Feb" / "Mar" / "Apr" /
                         "May" / "Jun" / "Jul" / "Aug" /
                         "Sep" / "Oct" / "Nov" / "Dec"
*)
let month =
  string "Jan" *> return Jan
  <|> string "Feb" *> return Feb
  <|> string "Mar" *> return Mar
  <|> string "Apr" *> return Apr
  <|> string "May" *> return May
  <|> string "Jun" *> return Jun
  <|> string "Jul" *> return Jul
  <|> string "Aug" *> return Aug
  <|> string "Sep" *> return Sep
  <|> string "Oct" *> return Oct
  <|> string "Nov" *> return Nov
  <|> string "Dec" *> return Dec

(* From RFC 822

     day         =  "Mon"  / "Tue" /  "Wed"  / "Thu"
                 /  "Fri"  / "Sat" /  "Sun"

   From RFC 2822

     day-name        =       "Mon" / "Tue" / "Wed" / "Thu" /
                             "Fri" / "Sat" / "Sun"

   From RFC 5322

     day-name        =   "Mon" / "Tue" / "Wed" / "Thu" /
                         "Fri" / "Sat" / "Sun"
*)
let day_name =
  string "Mon" *> return Mon
  <|> string "Tue" *> return Tue
  <|> string "Wed" *> return Wed
  <|> string "Thu" *> return Thu
  <|> string "Fri" *> return Fri
  <|> string "Sat" *> return Sat
  <|> string "Sun" *> return Sun

(* From RFC 2822

     obs-day-of-week =       [CFWS] day-name [CFWS]

   From RFC 5322

     obs-day-of-week =   [CFWS] day-name [CFWS]
*)
let obs_day_of_week =
  option () Rfc822.cfws *> day_name <* option () Rfc822.cfws

(* From RFC 2822

     day-of-week     =       ([FWS] day-name) / obs-day-of-week

   From RFC 5322

     day-of-week     =   ([FWS] day-name) / obs-day-of-week
*)
let day_of_week =
  obs_day_of_week <|> option (false, false, false) Rfc822.fws *> day_name

(* From RFC 822

     date        =  1*2DIGIT month 2DIGIT        ; day month year
                                                 ;  e.g. 20 Jun 82

   From RFC 2822

     date            =       day month year

   From RFC 5322

     date            =   day month year
*)
let date = lift3 (fun day month year -> (day, month, year)) (day <?> "day") (month <?> "month") (year <?> "year")

(* From RFC 822

     hour        =  2DIGIT ":" 2DIGIT [":" 2DIGIT]

   From RFC 2822

     time-of-day     =       hour ":" minute [ ":" second ]

   From RFC 5322

     time-of-day     =   hour ":" minute [ ":" second ]
*)
let time_of_day =
  hour <?> "hour"
  >>= fun hour ->
  char ':' *> minute <?> "minute"
  >>= fun minute ->
  option None
    (option () Rfc822.cfws *> char ':' *> second <?> "second" >>| fun second -> Some second)
  >>| fun second -> (hour, minute, second)

let is_military_zone = function
  | '\065' .. '\073' | '\075' .. '\090' | '\097' .. '\105' | '\107' .. '\122'
    ->
      true
  | _ -> false

(* From RFC 822

     zone        =  "UT"  / "GMT"                ; Universal Time
                                                 ; North American : UT
                 /  "EST" / "EDT"                ;  Eastern:  - 5/ - 4
                 /  "CST" / "CDT"                ;  Central:  - 6/ - 5
                 /  "MST" / "MDT"                ;  Mountain: - 7/ - 6
                 /  "PST" / "PDT"                ;  Pacific:  - 8/ - 7
                 /  1ALPHA                       ; Military: Z = UT;
                                                 ;  A:-1; (J not used)
                                                 ;  M:-12; N:+1; Y:+12
                 / ( ("+" / "-") 4DIGIT )        ; Local differential
                                                 ;  hours+min. (HHMM)

     Time zone may be indicated in several ways.  "UT" is Univer-
     sal  Time  (formerly called "Greenwich Mean Time"); "GMT" is per-
     mitted as a reference to Universal Time.  The  military  standard
     uses  a  single  character for each zone.  "Z" is Universal Time.
     "A" indicates one hour earlier, and "M" indicates 12  hours  ear-
     lier;  "N"  is  one  hour  later, and "Y" is 12 hours later.  The
     letter "J" is not used.  The other remaining two forms are  taken
     from ANSI standard X3.51-1975.  One allows explicit indication of
     the amount of offset from UT; the other uses  common  3-character
     strings for indicating time zones in North America.

   From RFC 2822

     obs-zone        =       "UT" / "GMT" /          ; Universal Time
                             "EST" / "EDT" /         ; Eastern:  - 5/ - 4
                             "CST" / "CDT" /         ; Central:  - 6/ - 5
                             "MST" / "MDT" /         ; Mountain: - 7/ - 6
                             "PST" / "PDT" /         ; Pacific:  - 8/ - 7
                                                     ;
                             %d65-73 /               ; Military zones - "A"
                             %d75-90 /               ; through "I" and "K"
                             %d97-105 /              ; through "Z", both
                             %d107-122               ; upper and lower case

     In the obsolete time zone, "UT" and "GMT" are indications of
     "Universal Time" and "Greenwich Mean Time" respectively and are both
     semantically identical to "+0000".

     The remaining three character zones are the US time zones.  The first
     letter, "E", "C", "M", or "P" stands for "Eastern", "Central",
     "Mountain" and "Pacific".  The second letter is either "S" for
     "Standard" time, or "D" for "Daylight" (or summer) time.  Their
     interpretations are as follows:

     EDT is semantically equivalent to -0400
     EST is semantically equivalent to -0500
     CDT is semantically equivalent to -0500
     CST is semantically equivalent to -0600
     MDT is semantically equivalent to -0600
     MST is semantically equivalent to -0700
     PDT is semantically equivalent to -0700
     PST is semantically equivalent to -0800

     The 1 character military time zones were defined in a non-standard
     way in [RFC822] and are therefore unpredictable in their meaning.
     The original definitions of the military zones "A" through "I" are
     equivalent to "+0100" through "+0900" respectively; "K", "L", and "M"
     are equivalent to  "+1000", "+1100", and "+1200" respectively; "N"
     through "Y" are equivalent to "-0100" through "-1200" respectively;
     and "Z" is equivalent to "+0000".  However, because of the error in
     [RFC822], they SHOULD all be considered equivalent to "-0000" unless
     there is out-of-band information confirming their meaning.

     Other multi-character (usually between 3 and 5) alphabetic time zones
     have been used in Internet messages.  Any such time zone whose
     meaning is not known SHOULD be considered equivalent to "-0000"
     unless there is out-of-band information confirming their meaning.

   From RFC 5322

     obs-zone        =   "UT" / "GMT" /     ; Universal Time
                                            ; North American UT
                                            ; offsets
                         "EST" / "EDT" /    ; Eastern:  - 5/ - 4
                         "CST" / "CDT" /    ; Central:  - 6/ - 5
                         "MST" / "MDT" /    ; Mountain: - 7/ - 6
                         "PST" / "PDT" /    ; Pacific:  - 8/ - 7
                                            ;
                         %d65-73 /          ; Military zones - "A"
                         %d75-90 /          ; through "I" and "K"
                         %d97-105 /         ; through "Z", both
                         %d107-122          ; upper and lower case

     Where a two or three digit year occurs in a date, the year is to be
     interpreted as follows: If a two digit year is encountered whose
     value is between 00 and 49, the year is interpreted by adding 2000,
     ending up with a value between 2000 and 2049.  If a two digit year is
     encountered with a value between 50 and 99, or any three digit year
     is encountered, the year is interpreted by adding 1900.

     In the obsolete time zone, "UT" and "GMT" are indications of
     "Universal Time" and "Greenwich Mean Time", respectively, and are
     both semantically identical to "+0000".

     The remaining three character zones are the US time zones.  The first
     letter, "E", "C", "M", or "P" stands for "Eastern", "Central",
     "Mountain", and "Pacific".  The second letter is either "S" for
     "Standard" time, or "D" for "Daylight Savings" (or summer) time.
     Their interpretations are as follows:

        EDT is semantically equivalent to -0400
        EST is semantically equivalent to -0500
        CDT is semantically equivalent to -0500
        CST is semantically equivalent to -0600
        MDT is semantically equivalent to -0600
        MST is semantically equivalent to -0700
        PDT is semantically equivalent to -0700
        PST is semantically equivalent to -0800

     The 1 character military time zones were defined in a non-standard
     way in [RFC0822] and are therefore unpredictable in their meaning.
     The original definitions of the military zones "A" through "I" are
     equivalent to "+0100" through "+0900", respectively; "K", "L", and
     "M" are equivalent to "+1000", "+1100", and "+1200", respectively;
     "N" through "Y" are equivalent to "-0100" through "-1200".
     respectively; and "Z" is equivalent to "+0000".  However, because of
     the error in [RFC0822], they SHOULD all be considered equivalent to
     "-0000" unless there is out-of-band information confirming their
     meaning.

     Other multi-character (usually between 3 and 5) alphabetic time zones
     have been used in Internet messages.  Any such time zone whose
     meaning is not known SHOULD be considered equivalent to "-0000"
     unless there is out-of-band information confirming their meaning.
*)
let obs_zone =
  string "UT" *> return UT
  <|> string "GMT" *> return GMT
  <|> string "EST" *> return EST
  <|> string "EDT" *> return EDT
  <|> string "CST" *> return CST
  <|> string "CDT" *> return CDT
  <|> string "MST" *> return MST
  <|> string "MDT" *> return MDT
  <|> string "PST" *> return PST
  <|> string "PDT" *> return PDT
  <|> (satisfy is_military_zone >>= fun z -> return (Military_zone z))

(* From RFC 2822

     zone            =       (( "+" / "-" ) 4DIGIT) / obs-zone

     The zone specifies the offset from Coordinated Universal Time (UTC,
     formerly referred to as "Greenwich Mean Time") that the date and
     time-of-day represent.  The "+" or "-" indicates whether the
     time-of-day is ahead of (i.e., east of) or behind (i.e., west of)
     Universal Time.  The first two digits indicate the number of hours
     difference from Universal Time, and the last two digits indicate the
     number of minutes difference from Universal Time.  (Hence, +hhmm
     means +(hh * 60 + mm) minutes, and -hhmm means -(hh * 60 + mm)
     minutes).  The form "+0000" SHOULD be used to indicate a time zone at
     Universal Time.  Though "-0000" also indicates Universal Time, it is
     used to indicate that the time was generated on a system that may be
     in a local time zone other than Universal Time and therefore
     indicates that the date-time contains no information about the local
     time zone.

   From RFC 5322

     zone            =   (FWS ( "+" / "-" ) 4DIGIT) / obs-zone

     The zone specifies the offset from Coordinated Universal Time (UTC,
     formerly referred to as "Greenwich Mean Time") that the date and
     time-of-day represent.  The "+" or "-" indicates whether the time-of-
     day is ahead of (i.e., east of) or behind (i.e., west of) Universal
     Time.  The first two digits indicate the number of hours difference
     from Universal Time, and the last two digits indicate the number of
     additional minutes difference from Universal Time.  (Hence, +hhmm
     means +(hh * 60 + mm) minutes, and -hhmm means -(hh * 60 + mm)
     minutes).  The form "+0000" SHOULD be used to indicate a time zone at
     Universal Time.  Though "-0000" also indicates Universal Time, it is
     used to indicate that the time was generated on a system that may be
     in a local time zone other than Universal Time and that the date-time
     contains no information about the local time zone.
*)
let zone =
  (* XXX(dinosaure): we clearly have a bug in this place. Indeed, ABNF expects
     an explicit space between [zone] and [time_of_day]. However, if we see
     [second] or [minute], they are surrounded by [CFWS]. That mean, they
     consume trailing spaces. If we explicitly expect [FWS] here, we will fail -
     mostly because this expected space is a part of [minute] or [second]. To
     avoid an error, [FWS] is optional but a better way should to check if we
     consumed at least one space before [zone]. *)
  (option (false, false, false) Rfc822.fws *> satisfy (function '+' | '-' -> true | _ -> false) <?> "sign"
   >>= (fun sign ->
       four_digit <?> "four-digit"
       >>| fun zone ->
       let one = if sign = '-' then - int_of_string (String.sub zone 0 2) else int_of_string (String.sub zone 0 2) in
       let two = int_of_string (String.sub zone 2 2) in
       TZ (one, two)))
  <|> (option () Rfc822.cfws *> obs_zone)

(* From RFC 822

     time        =  hour zone                    ; ANSI and Military

   From RFC 2822

     time            =       time-of-day FWS zone

   From RFC 5322

     time            =   time-of-day zone
*)
let time = lift2 (fun time zone -> (time, zone)) (time_of_day <?> "time-of-day") (zone <?> "zone")

(* From RFC 822

     date-time   =  [ day "," ] date time        ; dd mm yy
                                                 ;  hh:mm:ss zzz

   From RFC 2822

     Date and time occur in several header fields.  This section specifies
     the syntax for a full date and time specification.  Though folding
     white space is permitted throughout the date-time specification, it
     is RECOMMENDED that a single space be used in each place that FWS
     appears (whether it is required or optional); some older
     implementations may not interpret other occurrences of folding white
     space correctly.

     date-time       =       [ day-of-week "," ] date FWS time [CFWS]

   From RFC 5322

     Date and time values occur in several header fields.  This section
     specifies the syntax for a full date and time specification.  Though
     folding white space is permitted throughout the date-time
     specification, it is RECOMMENDED that a single space be used in each
     place that FWS appears (whether it is required or optional); some
     older implementations will not interpret longer sequences of folding
     white space correctly.

     date-time       =   [ day-of-week "," ] date time [CFWS]
*)
let date_time =
  lift3
    (fun day date (time, zone) -> {day; date; time; zone})
    (option None (day_of_week >>= fun day -> char ',' *> return (Some day)))
    date time
  <* option () Rfc822.cfws

(* From RFC 5322

     obs-utext       =   %d0 / obs-NO-WS-CTL / VCHAR
*)
let is_obs_utext = function
  | '\000' -> true
  | c -> Rfc822.is_obs_no_ws_ctl c || Rfc822.is_vchar c

(* From RFC 822

     B.  SIMPLE FIELD PARSING

          Some mail-reading software systems may wish to perform  only
     minimal  processing,  ignoring  the internal syntax of structured
     field-bodies and treating them the  same  as  unstructured-field-
     bodies.  Such software will need only to distinguish:

         o   Header fields from the message body,

         o   Beginnings of fields from lines which continue fields,

         o   Field-names from field-contents.

          The abbreviated set of syntactic rules  which  follows  will
     suffice  for  this  purpose.  It describes a limited view of mes-
     sages and is a subset of the syntactic rules provided in the main
     part of this specification.  One small exception is that the con-
     tents of field-bodies consist only of text:

     B.1.  SYNTAX


     message         =   *field *(CRLF *text)

     field           =    field-name ":" [field-body] CRLF

     field-name      =  1*<any CHAR, excluding CTLs, SPACE, and ":">

     field-body      =   *text [CRLF LWSP-char field-body]


     B.2.  SEMANTICS

          Headers occur before the message body and are terminated  by
     a null line (i.e., two contiguous CRLFs).

          A line which continues a header field begins with a SPACE or
     HTAB  character,  while  a  line  beginning a field starts with a
     printable character which is not a colon.

          A field-name consists of one or  more  printable  characters
     (excluding  colon,  space, and control-characters).  A field-name
     MUST be contained on one line.  Upper and lower case are not dis-
     tinguished when comparing field-names.

     NOTE: It's not clear if, in this case (instead [phrase]), [wsp] is
     significant. Currently, we did count how many [wsp] we consume by
     [cfws]/[fws] token. But I suspect it should be the case.

   From RFC 2047

     NOTE: I did find any description of [encoded-word] in [obs-unstruct].
     However, I wrote this kind for a good reason and I prefer to believe
     the 2 years ago dinosaure.

     (3) As a replacement for a 'word' entity within a 'phrase', for example,
         one that precedes an address in a From, To, or Cc header.  The ABNF
         definition for 'phrase' from RFC 822 thus becomes:

         phrase = 1 *( encoded-word / word )

         In this case the set of characters that may be used in a "Q"-encoded
         'encoded-word' is restricted to: <upper and lower case ASCII
         letters, decimal digits, "!", "*", "+", "-", "/", "=", and "_"
         (underscore, ASCII 95.)>.  An 'encoded-word' that appears within a
         'phrase' MUST be separated from any adjacent 'word', 'text' or
         'special' by 'linear-white-space'.

        These are the ONLY locations where an 'encoded-word' may appear.  In
        particular:

        + An 'encoded-word' MUST NOT appear in any portion of an 'addr-spec'.

        + An 'encoded-word' MUST NOT appear within a 'quoted-string'.

        + An 'encoded-word' MUST NOT be used in a Received header field.

        + An 'encoded-word' MUST NOT be used in parameter of a MIME
          Content-Type or Content-Disposition field, or in any structured
          field body except within a 'comment' or 'phrase'.

   From RFC 2822

     obs-text        =       *LF *CR *(obs-char *LF *CR)

     obs-char        =       %d0-9 / %d11 /          ; %d0-127 except CR and
                             %d12 / %d14-127         ;  LF

     obs-utext       =       obs-text

   From RFC 5322

     obs-unstruct    =   *(( *LF *CR *(obs-utext *LF *CR)) / FWS)

     The following are changes from [RFC2882]:
     15.  Simplified obs-qp.  Fixed and simplified obs-utext (which now
          only appears in the obsolete syntax).  Removed obs-text and obs-
          char, adding obs-body.
*)
let obs_unstruct : unstructured t =
  let many_cr =
    fix @@ fun m ->
    Unsafe.peek 2 (fun buf ~off ~len:_ ->
        match Bigstringaf.get buf (off + 0), Bigstringaf.get buf (off + 1) with
        | '\r', '\r' -> `CRCR
        | '\r', _ -> `CR
        | _, _ -> `Something)
    <|> ( peek_char
        >>| function
        | Some '\r' -> `CR | Some _ -> `Something | None -> `End_of_input )
    >>= function
    | `CRCR -> advance 1 *> m >>| fun x -> x + 1
    | `CR -> return 1
    | `Something -> return 0
    | `End_of_input -> return 0
  in
  let word =
    Rfc2047.encoded_word
    >>| (fun e -> `Encoded e)
    <|> (Rfc6532.with_uutf1 is_obs_utext >>| fun e -> `Text e)
  in
  let safe_lfcr =
    many (char '\n') >>| List.length
    >>= fun lf ->
    many_cr
    >>= fun cr -> (* At this stage, we have at least one [CR] in the input iff [cr > 0]. *)
    Unsafe.peek 2
    @@ fun buf ~off ~len ->
    let raw = Bigstringaf.substring buf ~off ~len in
    match (lf, cr, Bigstringaf.get buf (off + 1)) with
    | 0, 0, _ -> []
    | n, 0, _ -> [`LF n]
    | n, 1, '\n' ->
        assert (raw.[0] = '\r') ;
        if n <> 0 then [`LF n] else [ (* CRLF *) ]
    | n, m, '\n' ->
        assert (raw.[0] = '\r') ;
        if n <> 0 then [`LF n; `CR (m - 1); (* CRLF *) ] else [`CR (m - 1); (* CRLF *) ]
    | n, m, _ ->
        assert (raw.[0] = '\r') ; (* because [m > 0] *)
        [`LF n; `CR m]
  in
  many
    ( safe_lfcr
      >>= (fun pre ->
            many (word >>= fun word -> safe_lfcr >>| fun rst ->
                  word :: rst)
            >>= fun rst ->
            (match List.concat (pre :: rst) with
             | [] -> fail ""
             (* XXX(dinosaure): if we have nothing, we have at leat [CRLF]. So
                we fail to start the other branch with [fws] if we need to continue [many].
                Otherwise, we have [CRLF] with something else. *)
             | res -> return res) )
    <|> ( Rfc822.fws
        >>| function
        | true, true, true -> [`WSP; `CRLF; `WSP]
        | false, true, true -> [`CRLF; `WSP]
        | true, true, false -> [`WSP; `CRLF]
        | false, true, false -> [`CRLF]
        | true, false, true -> [`WSP; `WSP]
        | true, false, false | false, false, true -> [`WSP]
        | false, false, false -> [] ) )
  >>| List.concat

let make n f =
  let rec aux acc = function
    | 0 -> List.rev acc
    | n -> aux (f n :: acc) (n - 1)
  in
  aux [] n

(* From RFC 2822

     Some field bodies in this standard are defined simply as
     "unstructured" (which is specified below as any US-ASCII characters,
     except for CR and LF) with no further restrictions.  These are
     referred to as unstructured field bodies.  Semantically, unstructured
     field bodies are simply to be treated as a single line of characters
     with no further processing (except for header "folding" and
     "unfolding" as described in section 2.2.3).

     unstructured    =       *([FWS] utext) [FWS]

   From RFC 5322

     Some field bodies in this specification are defined simply as
     "unstructured" (which is specified in section 3.2.5 as any printable
     US-ASCII characters plus white space characters) with no further
     restrictions.  These are referred to as unstructured field bodies.
     Semantically, unstructured field bodies are simply to be treated as a
     single line of characters with no further processing (except for
     "folding" and "unfolding" as described in section 2.2.3).

     unstructured    =   ( *([FWS] VCHAR) *WSP ) / obs-unstruct
*)
let unstructured =
  obs_unstruct
  <|> ( many
          ( option (false, false, false) Rfc822.fws
          >>= fun (has_wsp, has_crlf, has_wsp') ->
          Rfc6532.with_uutf1 Rfc822.is_vchar
          (* XXX(dinosaure): currently, we use [with_uutf1]. However, RFC explains [VCHAR].
             I suspect an infinite loop if we use [with_uutf]. *)
          >>| fun text ->
          match (has_wsp, has_crlf, has_wsp') with
          | true, true, true -> [`WSP; `CRLF; `WSP; `Text text]
          | false, true, true -> [`CRLF; `WSP; `Text text]
          | true, true, false -> [`WSP; `CRLF; `Text text]
          | false, true, false -> [`CRLF; `Text text]
          | true, false, true -> [`WSP; `WSP; `Text text]
          | true, false, false | false, false, true -> [`WSP; `Text text]
          | false, false, false -> [`Text text] )
      >>= fun pre ->
      many (char '\x09' <|> char '\x20')
      >>| List.length
      >>| fun n -> List.concat pre @ make n (fun _ -> `WSP) )

let phrase_or_msg_id =
  many
    ((phrase >>| fun v -> `Phrase v)
     <|> (Rfc822.msg_id ~address_literal:(fail "Invalid domain") >>| fun v -> `MsgID v))

(* From RFC 2822

     obs-phrase-list =       phrase / 1*([phrase] [CFWS] "," [CFWS]) [phrase]

   From RFC 5322

     obs-phrase-list =   [phrase / CFWS] *("," [phrase / CFWS])

     The following are changes from [RFC2822]
     17.  Fixed all obsolete list syntax (obs-domain-list, obs-mbox-list,
           obs-addr-list, obs-phrase-list, and the newly added obs-group-
           list).
*)
let obs_phrase_list =
  option [] (phrase <|> Rfc822.cfws *> return [])
  >>= fun pre ->
  many (char ',' *> option [] (phrase <|> Rfc822.cfws *> return []))
  >>| fun rst -> pre :: rst

(* From RFC 2822

     keywords        =       "Keywords:" phrase *("," phrase) CRLF
     obs-keywords    =       "Keywords" *WSP ":" obs-phrase-list CRLF

   From RFC 5322

     keywords        =   "Keywords:" phrase *("," phrase) CRLF
     obs-keywords    =   "Keywords" *WSP ":" obs-phrase-list CRLF
*)
let keywords =
  let sep s p =
    fix (fun m -> lift2 (fun x r -> x :: r) p (s *> m <|> return []))
  in
  obs_phrase_list <|> sep (char ',') phrase

let is_ftext = function
  | '\033' .. '\057' | '\059' .. '\126' -> true
  | _ -> false

let implode l =
  let s = Bytes.create (List.length l) in
  let rec aux i = function
    | [] -> s
    | x :: r ->
        Bytes.set s i x ;
        aux (i + 1) r
  in
  aux 0 l

let received_token =
  addr_spec
  >>| (fun mailbox -> `Addr mailbox)
  <|> (angle_addr >>| fun mailbox -> `Addr mailbox)
  <|> (domain >>| fun domain -> `Domain domain)
  <|> (Rfc822.word >>| fun word -> `Word word)

let received =
  many received_token
  >>= fun lst ->
  option None (char ';' *> date_time >>| fun v -> Some v)
  >>| fun rst -> (lst, rst)

let path =
  angle_addr
  >>| (fun v -> Some v)
  <|> (option () Rfc822.cfws
       *> char '<'
       *> option () Rfc822.cfws
       *> char '>'
       *> option () Rfc822.cfws
       *> return None)
  <|> (addr_spec >>| fun mailbox -> Some mailbox)

let field_name = take_while1 is_ftext

let trace path =
  let r =
    string "Received"
    *> many (satisfy (function '\x09' | '\x20' -> true | _ -> false))
    *> char ':'
    *> received
    <* Rfc822.crlf
  in
  match path with
  | `ReturnPath path -> many r >>| fun traces -> (path, traces)
  (* XXX(dinosaure): in this case, RFC 5322 explicitly say we expect at least
     one [Received] field. However, it seems than some mails did not respect
     that. So we replace [many1] by [many] to do the best effort. *)
  | `Received ->
      received
      <* Rfc822.crlf
      >>= fun pre -> many r >>| fun rst -> (None, pre :: rst)

let field extend field_name =
  match String.lowercase_ascii field_name with
  | "date" -> date_time <* Rfc822.crlf >>| fun v -> `Date v
  | "from" -> mailbox_list <* Rfc822.crlf >>| fun v -> `From v
  | "sender" -> mailbox <* Rfc822.crlf >>| fun v -> `Sender v
  | "reply-to" -> address_list <* Rfc822.crlf >>| fun v -> `ReplyTo v
  | "to" -> address_list <* Rfc822.crlf >>| fun v -> `To v
  | "cc" -> address_list <* Rfc822.crlf >>| fun v -> `Cc v
  | "bcc" -> address_list <* Rfc822.crlf >>| fun v -> `Bcc v
  | "message-id" -> Rfc822.msg_id ~address_literal:(fail "Invalid domain") <* Rfc822.crlf >>| fun v -> `MessageID v
  | "in-reply-to" -> phrase_or_msg_id <* Rfc822.crlf >>| fun v -> `InReplyTo v
  | "references" -> phrase_or_msg_id <* Rfc822.crlf >>| fun v -> `References v
  | "subject" -> unstructured <* Rfc822.crlf >>| fun v -> `Subject v
  | "comments" -> unstructured <* Rfc822.crlf >>| fun v -> `Comments v
  | "keywords" -> keywords <* Rfc822.crlf >>| fun v -> `Keywords v
  | "resent-date" -> date_time <* Rfc822.crlf >>| fun v -> `ResentDate v
  | "resent-from" -> mailbox_list <* Rfc822.crlf >>| fun v -> `ResentFrom v
  | "resent-sender" -> mailbox <* Rfc822.crlf >>| fun v -> `ResentSender v
  | "resent-to" -> address_list <* Rfc822.crlf >>| fun v -> `ResentTo v
  | "resent-cc" -> address_list <* Rfc822.crlf >>| fun v -> `ResentCc v
  | "resent-bcc" -> address_list <* Rfc822.crlf >>| fun v -> `ResentBcc v
  | "resent-message-id" -> Rfc822.msg_id ~address_literal:(fail "Invalid domain") <* Rfc822.crlf >>| fun v -> `ResentMessageID v
  | "resent-reply-to" ->
      address_list <* Rfc822.crlf >>| fun v -> `ResentReplyTo v
  | "received" -> trace `Received >>| fun v -> `Trace v
  | "return-path" -> path <* Rfc822.crlf >>= fun v -> trace (`ReturnPath v) >>| fun v -> `Trace v
  | _ ->
      extend field_name
      <|> (unstructured <* Rfc822.crlf >>| fun v -> `Field (field_name, v))

let sp = Format.sprintf

let field extend field_name =
  field extend field_name
  <|> ( unstructured
      <* Rfc822.crlf
      >>| (fun v -> `Unsafe (field_name, v))
      <?> sp "Unsafe %s" field_name )

let skip_field =
  fix @@ fun m ->
  lift2
    (fun line -> function `Rest rest -> line :: rest | `CRLF -> [line])
    (take_while (( <> ) '\r'))
    ( Rfc822.fws *> m
    >>= (fun rest -> return (`Rest rest))
    <|> (Rfc822.crlf >>= fun () -> return `CRLF) )

let header extend =
  many
    ( field_name
    <* many (satisfy (function '\x09' | '\x20' -> true | _ -> false))
    <* char ':'
    >>= (fun field_name -> field extend field_name)
    <|> (skip_field >>| fun lines -> `Skip lines) )

let parser ~write_line end_of_body =
  let check_end_of_body =
    let expected_len = String.length end_of_body in
    Unsafe.peek expected_len
      (fun ba ~off ~len ->
         let raw = Bigstringaf.substring ba ~off ~len in
         String.equal raw end_of_body) in

  fix @@ fun m ->
  let choose chunk = function
    | true ->
      let chunk = Bytes.sub_string chunk 0 (Bytes.length chunk - 1) in
      write_line chunk ; commit
    | false ->
      Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0] ;
      write_line (Bytes.unsafe_to_string chunk) ;
      advance 1 *> m in

  Unsafe.take_while ((<>) end_of_body.[0]) Bigstringaf.substring
  >>= fun chunk ->
  let chunk' = Bytes.create (String.length chunk + 1) in
  Bytes.blit_string chunk 0 chunk' 0 (String.length chunk) ;
  check_end_of_body >>= choose chunk'

let with_buffer ?(end_of_line = "\n") end_of_body =
  let buf = Buffer.create 0x100 in
  let write_line x =
    Buffer.add_string buf x ;
    Buffer.add_string buf end_of_line in

  parser ~write_line end_of_body >>| fun () -> Buffer.contents buf
