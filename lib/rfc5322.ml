type phrase =
  [`Dot | `Word of Rfc822.word | `Encoded of Rfc2047.encoded_string] list

type domain =
  [ `Domain of string list
  | `Literal of string
  | `Addr of Rfc5321.literal_domain ]

type mailbox =
  {name: phrase option; local: Rfc822.local; domain: domain * domain list}

type group = {name: phrase; mailbox: mailbox list}
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
  | TZ of int

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
  | `Encoded of Rfc2047.encoded_string ]
  list

type phrase_or_msg_id = [`Phrase of phrase | `MsgID of Rfc822.msg_id]

type resent =
  [ `ResentDate of date
  | `ResentFrom of mailbox list
  | `ResentSender of mailbox
  | `ResentTo of address list
  | `ResentCc of address list
  | `ResentBcc of address list
  | `ResentMessageID of Rfc822.msg_id
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
  | `MessageID of Rfc822.msg_id
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

(* From RFC 822

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

let phrase = obs_phrase <|> many1 word_with_encoded_word
let display_name = phrase

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

let obs_route = obs_domain_list <* char ':'

let obs_angle_addr =
  option () Rfc822.cfws *> char '<' *> obs_route
  >>= fun domains ->
  addr_spec
  >>= fun {name; local; domain= domain, _; _} ->
  char '>' *> option () Rfc822.cfws
  >>| fun () -> {name; local; domain= (domain, domains)}

let angle_addr =
  obs_angle_addr
  <|> ( option () Rfc822.cfws *> char '<' *> addr_spec
      >>= fun {name; local; domain= domain, _; _} ->
      char '>' *> option () Rfc822.cfws
      >>| fun _ -> {name; local; domain= (domain, [])} )

let name_addr =
  option None (display_name >>| fun x -> Some x)
  >>= fun name -> angle_addr >>| fun {local; domain; _} -> {name; local; domain}

let mailbox = name_addr <|> addr_spec

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

let obs_group_list =
  many1 (option () Rfc822.cfws *> char ',') *> option () Rfc822.cfws

let mailbox_list =
  obs_mbox_list
  <|> (mailbox >>= fun x -> many (char ',' *> mailbox) >>| fun r -> x :: r)

let group_list =
  mailbox_list
  <|> (obs_group_list >>| fun () -> [])
  <|> (Rfc822.cfws >>| fun () -> [])

let group =
  display_name
  >>= fun name ->
  char ':' *> option [] group_list
  >>= fun lst ->
  char ';' *> option () Rfc822.cfws >>| fun _ -> {name; mailbox= lst}

let address =
  group >>| (fun g -> `Group g) <|> (mailbox >>| fun m -> `Mailbox m)

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

let obs_hour =
  option () Rfc822.cfws *> two_digit <* option () Rfc822.cfws >>| int_of_string

let obs_minute = option () Rfc822.cfws *> two_digit >>| int_of_string
let obs_second = option () Rfc822.cfws *> two_digit >>| int_of_string
let hour = obs_hour <|> (two_digit >>| int_of_string)
let minute = obs_minute <|> (two_digit >>| int_of_string)
let second = obs_second <|> (two_digit >>| int_of_string)

let obs_year =
  option () Rfc822.cfws *> at_least_n_digit 2
  <* option () Rfc822.cfws
  >>| int_of_string

let year =
  Rfc822.fws *> at_least_n_digit 4 <* Rfc822.fws >>| int_of_string <|> obs_year

let obs_day =
  option () Rfc822.cfws *> one_or_two_digit
  <* option () Rfc822.cfws
  >>| int_of_string

let day =
  obs_day
  <|> ( option (false, false, false) Rfc822.fws *> one_or_two_digit
      <* Rfc822.fws
      >>| int_of_string )

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

let day_name =
  string "Mon" *> return Mon
  <|> string "Tue" *> return Tue
  <|> string "Wed" *> return Wed
  <|> string "Thu" *> return Thu
  <|> string "Fri" *> return Fri
  <|> string "Sat" *> return Sat
  <|> string "Sun" *> return Sun

let obs_day_of_week =
  option () Rfc822.cfws *> day_name <* option () Rfc822.cfws

let day_of_week =
  obs_day_of_week <|> option (false, false, false) Rfc822.fws *> day_name

let date = lift3 (fun day month year -> (day, month, year)) day month year

let time_of_day =
  hour
  >>= fun hour ->
  char ':' *> minute
  >>= fun minute ->
  option None
    (option () Rfc822.cfws *> char ':' *> second >>| fun second -> Some second)
  >>| fun second -> (hour, minute, second)

let is_military_zone = function
  | '\065' .. '\073' | '\075' .. '\090' | '\097' .. '\105' | '\107' .. '\122'
    ->
      true
  | _ -> false

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

let zone =
  Rfc822.fws *> satisfy (function '+' | '-' -> true | _ -> false)
  >>= (fun sign ->
        four_digit
        >>| fun zone ->
        if sign = '-' then TZ (-int_of_string zone)
        else TZ (int_of_string zone) )
  <|> option () Rfc822.cfws *> obs_zone

let time = lift2 (fun time zone -> (time, zone)) time_of_day zone

let date_time =
  lift3
    (fun day date (time, zone) -> {day; date; time; zone})
    (option None (day_of_week >>= fun day -> char ',' *> return (Some day)))
    date time
  <* option () Rfc822.cfws

let is_obs_utext = function
  | '\000' -> true
  | c -> Rfc822.is_obs_no_ws_ctl c || Rfc822.is_vchar c

let obs_unstruct : unstructured t =
  let many_cr =
    fix
    @@ fun m ->
    Unsafe.peek 2 (fun buf ~off ~len ->
        match Bigstringaf.substring buf ~off ~len with
        | "\r\r" -> `CRCR
        | x -> if x.[0] = '\r' then `CR else `Something )
    <|> ( peek_char
        >>| function
        | Some '\r' -> `CR | Some _ -> `Something | None -> `End_of_input )
    >>= function
    | `CRCR -> advance 1 *> m >>| fun x -> x + 2
    | `CR -> return 1
    | `Something -> return 0
    | `End_of_input -> return 0
  in
  let word =
    Rfc2047.encoded_word
    >>| (fun e -> `Encoded e)
    <|> (Rfc6532.with_uutf is_obs_utext >>| fun e -> `Text e)
  in
  let safe_lfcr =
    many (char '\n')
    >>| List.length
    >>= fun lf ->
    many_cr
    >>= fun cr ->
    Unsafe.peek 2
    @@ fun buf ~off ~len ->
    let raw = Bigstringaf.substring buf ~off ~len in
    match (lf, cr, raw.[1]) with
    | 0, 0, _ -> []
    | n, 0, _ -> [`LF n]
    | n, 1, '\n' ->
        assert (raw.[0] = '\r') ;
        if n <> 0 then [`LF n] else []
    | n, m, '\n' ->
        assert (raw.[0] = '\r') ;
        if n <> 0 then [`LF n; `CR (m - 1)] else [`CR (m - 1)]
    | n, m, _ ->
        assert (raw.[0] = '\r') ;
        [`LF n; `CR m]
  in
  many
    ( safe_lfcr
    >>= (fun pre ->
          many (word >>= fun word -> safe_lfcr >>| fun rst -> word :: rst)
          >>| fun rst -> List.concat (pre :: rst) )
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

let unstructured =
  obs_unstruct
  <|> ( many
          ( option (false, false, false) Rfc822.fws
          >>= fun (has_wsp, has_crlf, has_wsp') ->
          Rfc6532.with_uutf1 Rfc822.is_vchar
          (* TODO: with_uutf1 or with_uutf? *)
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
    (phrase >>| (fun v -> `Phrase v) <|> (Rfc5321.msg_id >>| fun v -> `MsgID v))

let obs_phrase_list =
  option [] (phrase <|> Rfc822.cfws *> return [])
  >>= fun pre ->
  many (char ',' *> option [] (phrase <|> Rfc822.cfws *> return []))
  >>| fun rst -> pre :: rst

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
  <|> option () Rfc822.cfws
      *> char '<'
      *> option () Rfc822.cfws
      *> char '>'
      *> option () Rfc822.cfws
      *> return None
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
  (* we recognize Return-Path *)
  | Some path -> many1 r >>| fun traces -> (path, traces)
  (* we recognize Received *)
  | None ->
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
  | "message-id" -> Rfc5321.msg_id <* Rfc822.crlf >>| fun v -> `MessageID v
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
  | "resent-message-id" ->
      Rfc5321.msg_id <* Rfc822.crlf >>| fun v -> `ResentMessageID v
  | "resent-reply-to" ->
      address_list <* Rfc822.crlf >>| fun v -> `ResentReplyTo v
  | "received" -> trace None >>| fun v -> `Trace v
  | "return-path" ->
      path <* Rfc822.crlf >>= fun v -> trace (Some v) >>| fun v -> `Trace v
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
  fix
  @@ fun m ->
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
