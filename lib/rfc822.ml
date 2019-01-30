type word = [`Atom of string | `String of string]
type 'addr domain = [`Domain of string list | `Literal of string | `Addr of 'addr]
type local = word list
type 'addr msg_id = local * 'addr domain

[@@@warning "-37"]

module Nonsense : sig type t = private Nonsense end = struct type t = Nonsense end
type nonsense = Nonsense.t

[@@@warning "+37"]

(* From RFC 5234 (used in RFC 5322)

   VCHAR          =  %x21-7E
                          ; visible (printing) characters
*)
let is_vchar = function '\x21' .. '\x7e' -> true | _ -> false

(* From RFC 5322

   obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control
                       %d11 /             ;  characters that do not
                       %d12 /             ;  include the carriage
                       %d14-31 /          ;  return, line feed, and
                       %d127              ;  white space characters
*)
let is_obs_no_ws_ctl = function
  | '\001' .. '\008' | '\011' | '\012' | '\014' .. '\031' | '\127' -> true
  | _ -> false

(* From RFC 822

     ctext       =  <any CHAR excluding "(",     ; => may be folded
                     ")", BACKSLASH & CR, & including
                     linear-white-space>

   From RFC 1522

     5. Use of encoded-words in message headers

       (2) An encoded-word may appear within a comment delimited by "(" and
           ")", i.e., wherever a "ctext" is allowed.  More precisely, the
           RFC 822 ABNF definition for "comment" is amended as follows:

           comment = "(" *(ctext / quoted-pair / comment / encoded-word) ")"

           A "Q"-encoded encoded-word which appears in a comment MUST NOT
           contain the characters "(", ")" or DQUOTE encoded-word that
           appears in a "comment" MUST be separated from any adjacent
           encoded-word or "ctext" by linear-white-space.

     7. Conformance

       A mail reading program claiming compliance with this specification
       must be able to distinguish encoded-words from "text", "ctext", or
       "word"s, according to the rules in section 6, anytime they appear in
       appropriate places in message headers.  It must support both the "B"
       and "Q" encodings for any character set which it supports.  The
       program must be able to display the unencoded text if the character

   From RFC 2047

     Update from RFC 1522:
     + clarification: an 'encoded-word' may appear immediately following
       the initial "(" or immediately before the final ")" that delimits a
       comment, not just adjacent to "(" and ")" *within* *ctext.

   From RFC 2822

     ctext           =       NO-WS-CTL /     ; Non white space controls

                             %d33-39 /       ; The rest of the US-ASCII
                             %d42-91 /       ;  characters not including "(",
                             %d93-126        ;  ")", or BACKSLASH

   From RFC 5322

     ctext           =   %d33-39 /          ; Printable US-ASCII
                         %d42-91 /          ;  characters not including
                         %d93-126 /         ;  "(", ")", or BACKSLASH
                         obs-ctext
     obs-ctext       =   obs-NO-WS-CTL

     Update from RFC 2822
     + Removed NO-WS-CTL from ctext

   From RFC 5335

     ctext   =/  UTF8-xtra-char
     UTF8-xtra-char  =   UTF8-2 / UTF8-3 / UTF8-4
     UTF8-2          =   %xC2-DF UTF8-tail

     UTF8-3          =   %xE0 %xA0-BF UTF8-tail /
                         %xE1-EC 2(UTF8-tail) /
                         %xED %x80-9F UTF8-tail /
                         %xEE-EF 2(UTF8-tail)

     UTF8-4          =   %xF0 %x90-BF 2( UTF8-tail ) /
                         %xF1-F3 3( UTF8-tail ) /
                         %xF4 %x80-8F 2( UTF8-tail )

     UTF8-tail       =   %x80-BF

   From RFC 6532

     ctext   =/  UTF8-non-ascii
*)
let is_ctext = function
  | '\033' .. '\039' | '\042' .. '\091' | '\093' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

(* From RFC 822

     qtext       =  <any CHAR excepting DQUOTE,     ; => may be folded
                     BACKSLASH & CR, and including
                     linear-white-space>

   From RFC 2822

     qtext           =       NO-WS-CTL /     ; Non white space controls

                             %d33 /          ; The rest of the US-ASCII
                             %d35-91 /       ;  characters not including BACKSLASH
                             %d93-126        ;  or the quote character


   From RFC 5322

     qtext           =   %d33 /             ; Printable US-ASCII
                         %d35-91 /          ;  characters not including
                         %d93-126 /         ;  BACKSLASH or the quote character
                         obs-qtext
     obs-qtext       =   obs-NO-WS-CTL

   From RFC 5335

     See [is_ctext] for UTF8-xtra-char.

     utf8-qtext    =     qtext / UTF8-xtra-char

   From RFC 6532

     qtext   =/  UTF8-non-ascii
*)
let is_qtext = function
  | '\033' | '\035' .. '\091' | '\093' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

(* From RFC 822

     The ABNF of atext is not explicit from RFC 822 but the relic could be find here:

     atom        =  1*<any CHAR except specials, SPACE and CTLs>

   From RFC 2822

     atext           =       ALPHA / DIGIT / ; Any character except controls,
                             "!" / "#" /     ;  SP, and specials.
                             "$" / "%" /     ;  Used for atoms
                             "&" / "'" /
                             "*" / "+" /
                             "-" / "/" /
                             "=" / "?" /
                             "^" / "_" /
                             "`" / "{" /
                             "|" / "}" /
                             "~"

   From RFC 5322

     atext           =   ALPHA / DIGIT /    ; Printable US-ASCII
                         "!" / "#" /        ;  characters not including
                         "$" / "%" /        ;  specials.  Used for atoms.
                         "&" / "'" /
                         "*" / "+" /
                         "-" / "/" /
                         "=" / "?" /
                         "^" / "_" /
                         "`" / "{" /
                         "|" / "}" /
                         "~"

   From RFC 5335

     utf8-atext   =  ALPHA / DIGIT /
                     "!" / "#" /     ; Any character except
                     "$" / "%" /     ; controls, SP, and specials.
                     "&" / "'" /     ; Used for atoms.
                     "*" / "+" /
                     "-" / "/" /
                     "=" / "?" /
                     "^" / "_" /
                     "`" / "{" /
                     "|" / "}" /
                     "~" /
                     UTF8-xtra-char
     UTF8-xtra-char: see is_ctext

     This means that all the [RFC2822] constructs that build upon these
     will permit UTF-8 characters, including comments and quoted strings.
     We do not change the syntax of <atext> in order to allow UTF8
     characters in <addr-spec>.  This would also allow UTF-8 characters in
     <message-id>, which is not allowed due to the limitation described in
     Section 4.5.  Instead, <utf8-atext> is added to meet this
     requirement.

   From RFC 6532

     atext   =/  UTF8-non-ascii
*)
let is_atext = function
  | 'a' .. 'z'
   |'A' .. 'Z'
   |'0' .. '9'
   |'!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?'
   |'^' | '_' | '`' | '{' | '}' | '|' | '~' ->
      true
  | _ -> false

let is_cr = ( = ) '\r'
let is_lf = ( = ) '\n'
let is_d0 = ( = ) '\000'

(* From RFC 822

     LWSP-char   =  SPACE / HTAB                 ; semantics = SPACE

   From RFC 2822 and RFC 5322, we did not find any occurrence of LWSP-char, it
   replaced by WSP. However, these RFCs does not provide an ABNF to describe
   WSP.
*)
let is_wsp = function '\x09' | '\x20' -> true | _ -> false

(* From RFC 822

     quoted-pair =  BACKSLASH CHAR                     ; may quote any char

     CHAR is case-sensitive

   From RFC 2822

     quoted-pair     =       (BACKSLASH text) / obs-qp
     text            =       %d1-9 /         ; Characters excluding CR and LF
                             %d11 /
                             %d12 /
                             %d14-127 /
                             obs-text
     obs-text        =       *LF *CR *(obs-char *LF *CR)
     obs-char        =       %d0-9 / %d11 /          ; %d0-127 except CR and
                             %d12 / %d14-127         ;  LF
     obs-qp          =       BACKSLASH (%d0-127)

   From RFC 5322

     quoted-pair     =   (BACKSLASH (VCHAR / WSP)) / obs-qp
     obs-qp          =   BACKSLASH (%d0 / obs-NO-WS-CTL / LF / CR)

   From RFC 5335

     See [is_ctext] for UTF8-xtra-char.

     utf8-text   =  %d1-9 /         ; all UTF-8 characters except
                    %d11-12 /       ; US-ASCII NUL, CR, and LF
                    %d14-127 /
                    UTF8-xtra-char
     utf8-quoted-pair   = (BACKSLASH utf8-text) / obs-qp
*)
let is_quoted_pair chr =
  is_vchar chr
  || is_wsp chr
  || is_d0 chr
  || is_obs_no_ws_ctl chr
  || is_lf chr
  || is_cr chr

(* From RFC 822

     dtext       =  <any CHAR excluding "[",     ; => may be folded
                "]", BACKSLASH & CR, & including
                linear-white-space>

   From RFC 2822

     dtext           =       NO-WS-CTL /     ; Non white space controls

                             %d33-90 /       ; The rest of the US-ASCII
                             %d94-126        ;  characters not including "[",
                                             ;  "]", or BACKSLASH

   From RFC 5322

     Update from RFC 2822:
     + Removed NO-WS-CTL from dtext

     dtext           =   %d33-90 /          ; Printable US-ASCII
                         %d94-126 /         ;  characters not including
                         obs-dtext          ;  "[", "]", or BACKSLASH
     obs-dtext       =   obs-NO-WS-CTL / quoted-pair
*)
let is_dtext = function
  | '\033' .. '\090' | '\094' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let of_escaped_character = function
  | '\x61' -> '\x07' (* "\a" *)
  | '\x62' -> '\x08' (* "\b" *)
  | '\x74' -> '\x09' (* "\t" *)
  | '\x6E' -> '\x0A' (* "\n" *)
  | '\x76' -> '\x0B' (* "\v" *)
  | '\x66' -> '\x0C' (* "\f" *)
  | '\x72' -> '\x0D' (* "\r" *)
  | c -> c

open Angstrom

(* See [is_quoted_pair] *)
let quoted_pair = char '\\' *> satisfy is_quoted_pair >>| of_escaped_character
let wsp = satisfy is_wsp
let crlf = char '\r' *> char '\n' *> return ()

(* From RFC 822

     Each header field can be viewed as a single, logical  line  of
     ASCII  characters,  comprising  a field-name and a field-body.
     For convenience, the field-body  portion  of  this  conceptual
     entity  can be split into a multiple-line representation; this
     is called "folding".  The general rule is that wherever  there
     may  be  linear-white-space  (NOT  simply  LWSP-chars), a CRLF
     immediately followed by AT LEAST one LWSP-char may instead  be
     inserted.  Thus, the single line

         To:  "Joe & J. Harvey" <ddd @Org>, JJV @ BBN

     can be represented as:

         To:  "Joe & J. Harvey" <ddd @ Org>,
                 JJV@BBN

     and

         To:  "Joe & J. Harvey"
                         <ddd@ Org>, JJV
          @BBN

     and

         To:  "Joe &
          J. Harvey" <ddd @ Org>, JJV @ BBN

          The process of moving  from  this  folded   multiple-line
     representation  of a header field to its single line represen-
     tation is called "unfolding".  Unfolding  is  accomplished  by
     regarding   CRLF   immediately  followed  by  a  LWSP-char  as
     equivalent to the LWSP-char.

     Note:  While the standard  permits  folding  wherever  linear-
            white-space is permitted, it is recommended that struc-
            tured fields, such as those containing addresses, limit
            folding  to higher-level syntactic breaks.  For address
            fields, it  is  recommended  that  such  folding  occur
            between addresses, after the separating comma.

   From RFC 2822

     White space characters, including white space used in folding
     (described in section 2.2.3), may appear between many elements in
     header field bodies.  Also, strings of characters that are treated as
     comments may be included in structured field bodies as characters
     enclosed in parentheses.  The following defines the folding white
     space (FWS) and comment constructs.

     Strings of characters enclosed in parentheses are considered comments
     so long as they do not appear within a "quoted-string", as defined in
     section 3.2.5.  Comments may nest.

     There are several places in this standard where comments and FWS may
     be freely inserted.  To accommodate that syntax, an additional token
     for "CFWS" is defined for places where comments and/or FWS can occur.
     However, where CFWS occurs in this standard, it MUST NOT be inserted
     in such a way that any line of a folded header field is made up
     entirely of WSP characters and nothing else.

     FWS             =       ([*WSP CRLF] 1*WSP) /   ; Folding white space
                             obs-FWS

     In the obsolete syntax, any amount of folding white space MAY be
     inserted where the obs-FWS rule is allowed.  This creates the
     possibility of having two consecutive "folds" in a line, and
     therefore the possibility that a line which makes up a folded header
     field could be composed entirely of white space.

     obs-FWS         =       1*WSP *(CRLF 1*WSP)

   From RFC 5322

     White space characters, including white space used in folding
     (described in section 2.2.3), may appear between many elements in
     header field bodies.  Also, strings of characters that are treated as
     comments may be included in structured field bodies as characters
     enclosed in parentheses.  The following defines the folding white
     space (FWS) and comment constructs.

     Strings of characters enclosed in parentheses are considered comments
     so long as they do not appear within a "quoted-string", as defined in
     section 3.2.4.  Comments may nest.

     There are several places in this specification where comments and FWS
     may be freely inserted.  To accommodate that syntax, an additional
     token for "CFWS" is defined for places where comments and/or FWS can
     occur.  However, where CFWS occurs in this specification, it MUST NOT
     be inserted in such a way that any line of a folded header field is
     made up entirely of WSP characters and nothing else.

     FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
                                            ; Folding white space

     In the obsolete syntax, any amount of folding white space MAY be
     inserted where the obs-FWS rule is allowed.  This creates the
     possibility of having two consecutive "folds" in a line, and
     therefore the possibility that a line which makes up a folded header
     field could be composed entirely of white space.

     obs-FWS         =   1*WSP *(CRLF 1*WSP)
*)
type wsp = HT | SP
type surrounded = { l : wsp list; r : wsp list }
type fws =
  | CRLF of surrounded list
  | L of wsp list
  | R of wsp list

let obs_fws =
  let to_list = List.map (function '\x09'-> HT | '\x20' -> SP | _ -> assert false) in
  many1 wsp >>| to_list >>= fun l -> many (crlf *> (many1 wsp >>| to_list))
  >>| function
  | [] -> L l
  | rs ->
    let res = List.mapi (fun i r -> if i = 0 then { l; r; } else { l= []; r }) rs in
    CRLF res

let fws =
  let to_list = List.map (function '\x09'-> HT | '\x20' -> SP | _ -> assert false) in
  obs_fws
  <|> ( option None (many wsp >>| to_list >>| Option.some <* crlf)
        >>= fun ls -> many1 wsp >>| to_list >>= fun r -> match ls with
        | None -> return (R r)
        | Some l -> return (CRLF [ { l; r; } ]) )

(* From RFC 822

     comment     =  "(" *(ctext / quoted-pair / comment) ")"

   From RFC 2822

     ccontent        =       ctext / quoted-pair / comment
     comment         =       "(" *([FWS] ccontent) [FWS] ")"

   From RFC 5322

     ccontent        =   ctext / quoted-pair / comment
     comment         =   "(" *([FWS] ccontent) [FWS] ")"
*)

type comment_content =
  | Text of Rfc6532.t
  | Quoted_pair of char
  | Comment of comment
and comment_content_surrounded = fws option * comment_content
and comment = comment_content_surrounded list * fws option

let failf fmt = Fmt.kstrf fail fmt
let ignore p = p *> return ()

let comment =
  let quoted_pair = quoted_pair >>| fun chr -> Quoted_pair chr in
  let ctext = Rfc6532.with_uutf1 is_ctext in
  (* XXX(dinosaure): [with_uutf1] needs an explanation when ABNF does not say
     [1*ctext]. Indeed, if we use [with_uutf], [ccontent] will never fail
     (specifically about empty comment).

     However, [many] is a loop and we need to leave it to check then the close
     parenthesis. The only way to leave [many] is to fail and it's why we expect
     at least one character. In other case [many (option (false, false, false)
     fws) *> ccontent] fails and we expected a close parenthesis. *)
  fix @@ fun comment ->
  let ccontent =
    peek_char
    >>= function
    | Some '(' -> comment >>= fun comment -> return (Comment comment)
    | Some '\\' -> quoted_pair
    | Some _ -> ctext >>= fun text -> return (Text text)
    | None -> failf "<comment> reaches end of input"
  in
  char '('
  *> many (option None (fws >>| Option.some)
           >>= fun lfws -> ccontent
           >>| fun comment_content -> (lfws, comment_content))
  >>= fun lst -> option None (fws >>| Option.some)
  >>= fun rfws -> char ')'
  *> return (lst, rfws)

let ignore_fws = ignore fws

(*
let comment =
  let ignore_quoted_pair = quoted_pair *> return () in
  let ignore_is_utf8_ctext = Rfc6532.with_uutf is_ctext *> return () in
  (fix @@ fun comment ->
    let ccontent : unit t =
      peek_char_fail
      >>= function
      | '(' -> comment
      | '\\' -> ignore_quoted_pair
      | c when is_ctext c -> ignore_is_utf8_ctext
      | _ -> fail "comment"
    in
    char '('
    *> (many ((option (false, false, false) fws) *> ccontent))
    *> (option (false, false, false) fws)
    *> char ')'
    *> return ())
*)

(* From RFC 822

     See [obs_fws] and [fws].

   From RFC 2822

     CFWS            =       *([FWS] comment) (([FWS] comment) / FWS)

   From RFC 5322

     CFWS            =   (1*([FWS] comment) [FWS]) / FWS

     Update from RFC 2822:
     + Simplified CFWS syntax.
*)

type cfws =
  | Comment of (fws option * comment) list * fws option
  | FWS of fws

let cfws =
  ( many1 (option None (fws >>| Option.some) >>= fun lfws -> comment >>= fun comment -> return (lfws, comment))
    >>= fun lst -> option None (fws >>| Option.some)
    >>= fun rfws -> return (Comment (lst, rfws)) )
  <|> (fws >>| fun fws -> FWS fws)

(* XXX(dinosaure): ok, at this stage we LOST comments! *)
let cfws = ignore cfws

(* From RFC 822

     The ABNF of qcontent is not explicit from RFC 822 but the relic could be find here:

     quoted-string = <"> *(qtext/quoted-pair) <">; Regular qtext or

   From RFC 2822

     qcontent        =       qtext / quoted-pair

   From RFC 5322

     qcontent        =   qtext / quoted-pair

   From RFC 5335

     utf8-qcontent      = utf8-qtext / utf8-quoted-pair
*)
let qcontent = Rfc6532.with_uutf1_without_raw is_qtext <|> (quoted_pair >>| String.make 1)

(* From RFC 822

     quoted-string = DQUOTE *(qtext/quoted-pair) DQUOTE; Regular qtext or
                                                       ;   quoted chars.

   From RFC 2047

     + An 'encoded-word' MUST NOT appear within a 'quoted-string'

   From RFC 2822

     quoted-string   =       [CFWS]
                             DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                             [CFWS]

     A quoted-string is treated as a unit.  That is, quoted-string is
     identical to atom, semantically.  Since a quoted-string is allowed to
     contain FWS, folding is permitted.  Also note that since quoted-pair
     is allowed in a quoted-string, the quote and backslash characters may
     appear in a quoted-string so long as they appear as a quoted-pair.

     Semantically, neither the optional CFWS outside of the quote
     characters nor the quote characters themselves are part of the
     quoted-string; the quoted-string is what is contained between the two
     quote characters.  As stated earlier, the BACKSLASH in any quoted-pair
     and the CRLF in any FWS/CFWS that appears within the quoted-string are
     semantically "invisible" and therefore not part of the quoted-string
     either.

     XXX(dinosaure): in other words, space(s) in [FWS] are "visible" between
     DQUOTE.

   From RFC 5322

     quoted-string   =   [CFWS]
                         DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                         [CFWS]

     The explanation does not change from RFC 2822.

   XXX(dinosaure): currently, this implementation has a bug about multiple
   spaces in [quoted-string]. We need to update [fws] to count how many space(s)
   we skip.
*)
let quoted_string =
  let fws_to_wsp fws =
    let res = Buffer.create 16 in
    let add_wsp =
      List.iter (function SP -> Buffer.add_char res ' '
                        | HT -> Buffer.add_char res '\t') in
    let () = match fws with
      | R lst
      | L lst -> add_wsp lst
      (* XXX(dinosaure): CRLF in any FWS/CFWS that appears within the
         quoted-string are semantically "invisible". *)
      | CRLF lst -> List.iter (fun { l; r; } -> add_wsp l ; add_wsp r) lst in
    Buffer.contents res in

  option () cfws
  *> char '"'
  *> ( many
         (option "" (fws >>| fws_to_wsp)
         >>= fun lfws -> qcontent
         >>= fun s -> return (lfws ^ s))
     >>= fun pre -> option "" (fws >>| fws_to_wsp)
     >>= fun rfws -> return (rfws :: pre))
  <* char '"'
  >>| String.concat ""
  <* option () cfws

(* From RFC 822

     atom        =  1*<any CHAR except specials, SPACE and CTLs>

     Difference from RFC 733:
     - Atoms may not contain SPACE.

   From RFC 2822

     atom            =       [CFWS] 1*atext [CFWS]

   From RFC 5322

     atom            =   [CFWS] 1*atext [CFWS]

   From RFC 5335

     utf8-atom     = [CFWS] 1*utf8-atext [CFWS]
*)
let atom = option () cfws *> Rfc6532.with_uutf1_without_raw is_atext <* option () cfws

(* From RFC 822

     word        =  atom / quoted-string

   From RFC 2822

     word            =       atom / quoted-string

   From RFC 5322

     word            =   atom / quoted-string
*)
let word =
  atom >>| (fun s -> `Atom s) <|> (quoted_string >>| fun s -> `String s)

(* From RFC 2822

     dot-atom-text   =       1*atext *("." 1*atext)

   From RFC 5322

     dot-atom-text   =   1*atext *("." 1*atext)
*)
let dot_atom_text = sep_by1 (char '.') (Rfc6532.with_uutf1_without_raw is_atext)

(* From RFC 2822

     dot-atom        =       [CFWS] dot-atom-text [CFWS]

   From RFC 5322

     dot-atom        =   [CFWS] dot-atom-text [CFWS]
*)
let dot_atom = option () cfws *> dot_atom_text <* option () cfws

(* From RFC 822

     local-part  =  word *("." word)             ; uninterpreted
                                                 ; case-preserved

     The local-part of an  addr-spec  in  a  mailbox  specification
     (i.e.,  the  host's  name for the mailbox) is understood to be
     whatever the receiving mail protocol server allows.  For exam-
     ple,  some systems do not understand mailbox references of the
     form "P. D. Q. Bach", but others do.

     This specification treats periods (".") as lexical separators.
     Hence,  their  presence  in  local-parts which are not quoted-
     strings, is detected.   However,  such  occurrences  carry  NO
     semantics.  That is, if a local-part has periods within it, an
     address parser will divide the local-part into several tokens,
     but  the  sequence  of  tokens will be treated as one uninter-
     preted unit.  The sequence  will  be  re-assembled,  when  the
     address is passed outside of the system such as to a mail pro-
     tocol service.

     For example, the address:

                        First.Last@Registry.Org

     is legal and does not require the local-part to be  surrounded
     with  quotation-marks.   (However,  "First  Last" DOES require
     quoting.)  The local-part of the address, when passed  outside
     of  the  mail  system,  within  the  Registry.Org  domain,  is
     "First.Last", again without quotation marks.

   From RFC 2822

     local-part      =       dot-atom / quoted-string / obs-local-part
     obs-local-part  =       word *("." word)

     The local-part portion is a domain dependent string.  In addresses,
     it is simply interpreted on the particular host as a name of a
     particular mailbox.

     Update:
     + CFWS within local-parts and domains not allowed.*

   From RFC 5322

     local-part      =   dot-atom / quoted-string / obs-local-part
     obs-local-part  =   word *("." word)

   XXX(dinosaure): local-part MUST not be empty.
*)
let obs_local_part = sep_by1 (char '.') word

let local_part =
  obs_local_part
  <|> (dot_atom >>| List.map (fun x -> `Atom x))
  <|> (quoted_string >>| fun s -> [`String s])

let local_part =
  local_part >>= fun x ->
    if List.fold_left (fun a -> function
        | `Atom x -> a + String.length x
        | `String x -> a + String.length x) 0 x = 0
    then fail "empty local-part"
    else return x

let obs_domain = lift2 (fun x r -> x :: r) atom (many1 (char '.' *> atom))

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
*)
let domain_literal =
  option () cfws
  *> char '['
  *> ( many
         ( option () ignore_fws
         *> (Rfc6532.with_uutf1_without_raw is_dtext <|> (quoted_pair >>| String.make 1))
         )
     >>| String.concat "" )
  <* option () ignore_fws
  <* char ']'
  <* option () cfws

(* From RFC 822

     domain      =  sub-domain *("." sub-domain)
     sub-domain  =  domain-ref / domain-literal
     domain-ref  =  atom                         ; symbolic reference

     6.2.1.  DOMAINS

     A name-domain is a set of registered (mail)  names.   A  name-
     domain  specification  resolves  to  a subordinate name-domain
     specification  or  to  a  terminal  domain-dependent   string.
     Hence,  domain  specification  is  extensible,  permitting any
     number of registration levels.

     Name-domains model a global, logical, hierarchical  addressing
     scheme.   The  model is logical, in that an address specifica-
     tion is related to name registration and  is  not  necessarily
     tied  to  transmission  path.   The  model's  hierarchy  is  a
     directed graph, called an in-tree, such that there is a single
     path  from  the root of the tree to any node in the hierarchy.
     If more than one path actually exists, they are considered  to
     be different addresses.

     The root node is common to all addresses; consequently, it  is
     not  referenced.   Its  children  constitute "top-level" name-
     domains.  Usually, a service has access to its own full domain
     specification and to the names of all top-level name-domains.

     The "top" of the domain addressing hierarchy -- a child of the
     root  --  is  indicated  by  the right-most field, in a domain
     specification.  Its child is specified to the left, its  child
     to the left, and so on.

     Some groups provide formal registration services;  these  con-
     stitute   name-domains   that  are  independent  logically  of
     specific machines.  In addition, networks and machines  impli-
     citly  compose name-domains, since their membership usually is
     registered in name tables.

     In the case of formal registration, an organization implements
     a  (distributed)  data base which provides an address-to-route
     mapping service for addresses of the form:

                      person@registry.organization

     Note that "organization" is a logical  entity,  separate  from
     any particular communication network.

     A mechanism for accessing "organization" is universally avail-
     able.   That mechanism, in turn, seeks an instantiation of the
     registry; its location is not indicated in the address specif-
     ication.   It  is assumed that the system which operates under
     the name "organization" knows how to find a subordinate regis-
     try.  The registry will then use the "person" string to deter-
     mine where to send the mail specification.

     The latter,  network-oriented  case  permits  simple,  direct,
     attachment-related address specification, such as:

                           user@host.network

     Once the network is accessed, it is expected  that  a  message
     will  go  directly  to the host and that the host will resolve
     the user name, placing the message in the user's mailbox.

     6.2.2.  ABBREVIATED DOMAIN SPECIFICATION

     Since any number of  levels  is  possible  within  the  domain
     hierarchy,  specification  of  a  fully  qualified address can
     become inconvenient.  This standard permits abbreviated domain
     specification, in a special case:

         For the address of  the  sender,  call  the  left-most
         sub-domain  Level  N.   In a header address, if all of
         the sub-domains above (i.e., to the right of) Level  N
         are  the same as those of the sender, then they do not
         have to appear in the specification.   Otherwise,  the
         address must be fully qualified.

         This feature is subject  to  approval  by  local  sub-
         domains.   Individual  sub-domains  may  require their
         member systems, which originate mail, to provide  full
         domain  specification only.  When permitted, abbrevia-
         tions may be present  only  while  the  message  stays
         within the sub-domain of the sender.

         Use of this mechanism requires the sender's sub-domain
         to reserve the names of all top-level domains, so that
         full specifications can be distinguished from abbrevi-
         ated specifications.

     For example, if a sender's address is:

              sender@registry-A.registry-1.organization-X

     and one recipient's address is:

             recipient@registry-B.registry-1.organization-X

     and another's is:

             recipient@registry-C.registry-2.organization-X

     then ".registry-1.organization-X" need not be specified in the
     the  message,  but  "registry-C.registry-2"  DOES  have  to be
     specified.  That is, the first two addresses may  be  abbrevi-
     ated, but the third address must be fully specified.

     When a message crosses a domain boundary, all  addresses  must
     be  specified  in  the  full format, ending with the top-level
     name-domain in the right-most field.  It is the responsibility
     of  mail  forwarding services to ensure that addresses conform
     with this requirement.  In the case of abbreviated  addresses,
     the  relaying  service must make the necessary expansions.  It
     should be noted that it often is difficult for such a  service
     to locate all occurrences of address abbreviations.  For exam-
     ple, it will not be possible to find such abbreviations within
     the  body  of  the  message.   The "Return-Path" field can aid
     recipients in recovering from these errors.

     Note:  When passing any portion of an addr-spec onto a process
            which  does  not interpret data according to this stan-
            dard (e.g., mail protocol servers).  There must  be  NO
            LWSP-chars  preceding  or  following the at-sign or any
            delimiting period ("."), such as  shown  in  the  above
            examples,   and   only  ONE  SPACE  between  contiguous
            <word>s.

     6.2.3.  DOMAIN TERMS

     A domain-ref must be THE official name of a registry, network,
     or  host.   It  is  a  symbolic  reference, within a name sub-
     domain.  At times, it is necessary to bypass standard  mechan-
     isms  for  resolving  such  references,  using  more primitive
     information, such as a network host address  rather  than  its
     associated host name.

     To permit such references, this standard provides the  domain-
     literal  construct.   Its contents must conform with the needs
     of the sub-domain in which it is interpreted.

     Domain-literals which refer to domains within the ARPA  Inter-
     net  specify  32-bit  Internet addresses, in four 8-bit fields
     noted in decimal, as described in Request for  Comments  #820,
     "Assigned Numbers."  For example:

                              [10.0.3.19]

     Note:  THE USE OF DOMAIN-LITERALS IS STRONGLY DISCOURAGED.  It
            is  permitted  only  as  a means of bypassing temporary
            system limitations, such as name tables which  are  not
            complete.

     The names of "top-level" domains, and  the  names  of  domains
     under  in  the  ARPA Internet, are registered with the Network
     Information Center, SRI International, Menlo Park, California.

   From RFC 2822

     domain          =       dot-atom / domain-literal / obs-domain
     obs-domain      =       atom *("." atom)

     Update:
     + CFWS within local-parts and domains not allowed.*

   From RFC 5322

     domain          =   dot-atom / domain-literal / obs-domain
     obs-domain      =   atom *("." atom)
*)
let domain ~address_literal =
  let of_string ~error p s =
    match parse_string p s with Ok v -> return v | Error _ -> fail error
  in
  let literal domain = failf "invalid literal domain: %s" domain in
  (* XXX(dinosaure): according to RFC 5322 and RFC 822 (including RFC 2822), a
     [`Literal] is a correct domain. However, according RFC 5321, we abort this
     kind of domain.

     [Mrmime] will be use with a SMTP/IMAP/POP protocol, we decide to fail on
     [`Literal]. This is an arbitrary choice. *)
  let addr s =
    of_string ~error:"address-literal" address_literal s
    >>| (fun addr -> `Addr addr)
    <|> literal s
  in
  (obs_domain >>| (fun domain -> `Domain domain))
  <|> (domain_literal >>= addr)
  <|> (dot_atom >>| fun domain -> `Domain domain)

(* From RFC 2822

     obs-id-left     =       local-part
     no-fold-quote   =       DQUOTE *(qtext / quoted-pair) DQUOTE
     id-left         =       dot-atom-text / no-fold-quote / obs-id-left

   From RFC 5322

     id-left         =   dot-atom-text / obs-id-left
     obs-id-left     =   local-part

   XXX(dinosaure): we took the RFC 5322's ABNF, the [no-fold-quote] token
   is available on the local-part as quoted-string.
*)
let id_left = local_part <|> (dot_atom_text >>| List.map (fun x -> `Atom x))

(* From RFC 2822

     no-fold-literal =       "[" *(dtext / quoted-pair) "]"

   From RFC 5322

     no-fold-literal =   "[" *dtext "]"
*)
let no_fold_literal = char '[' *> Rfc6532.with_uutf_without_raw is_dtext <* char ']'

(* From RFC 2822

     id-right        =       dot-atom-text / no-fold-literal / obs-id-right
     obs-id-right    =       domain

   From RFC 5322

     id-right        =   dot-atom-text / no-fold-literal / obs-id-right
     obs-id-right    =   domain
*)
let id_right ~address_literal =
  no_fold_literal
  >>| (fun literal -> `Literal literal)
  <|> domain ~address_literal
  <|> (dot_atom_text >>| fun domain -> `Domain domain)

(* From RFC 822

     addr-spec   =  local-part "@" domain        ; global address
     msg-id      =  "<" addr-spec ">"            ; Unique message id

   From RFC 2822

     msg-id          =       [CFWS] "<" id-left "@" id-right ">" [CFWS]

     Update:
     + CFWS within msg-id not allowed.*

     The message identifier (msg-id) is similar in syntax to an angle-addr
     construct without the internal CFWS.

   From RFC 5322

     msg-id          =   [CFWS] "<" id-left "@" id-right ">" [CFWS]

     Update:
     + Removed no-fold-quote from msg-id.  Clarified syntax

     The message identifier (msg-id) itself MUST be a globally unique
     identifier for a message.  The generator of the message identifier
     MUST guarantee that the msg-id is unique.  There are several
     algorithms that can be used to accomplish this.  Since the msg-id has
     a similar syntax to addr-spec (identical except that quoted strings,
     comments, and folding white space are not allowed), a good method is
     to put the domain name (or a domain literal IP address) of the host
     on which the message identifier was created on the right-hand side of
     the "@" (since domain names and IP addresses are normally unique),
     and put a combination of the current absolute date and time along
     with some other currently unique (perhaps sequential) identifier
     available on the system (for example, a process id number) on the
     left-hand side.  Though other algorithms will work, it is RECOMMENDED
     that the right-hand side contain some domain identifier (either of
     the host itself or otherwise) such that the generator of the message
     identifier can guarantee the uniqueness of the left-hand side within
     the scope of that domain.

     Semantically, the angle bracket characters are not part of the
     msg-id; the msg-id is what is contained between the two angle bracket
     characters.
*)
let msg_id ~address_literal =
  option () cfws *> char '<' *> id_left
  >>= fun left ->
  char '@' *> id_right ~address_literal
  >>= fun right -> char '>' *> option () cfws >>| fun () -> (left, right)
