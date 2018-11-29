type literal_domain =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Ext of string * string

open Angstrom

(* From RFC 5321

     Let-dig        = ALPHA / DIGIT
     Ldh-str        = *( ALPHA / DIGIT / "-" ) Let-dig
     address-literal  = "[" ( IPv4-address-literal /
                      IPv6-address-literal /
                      General-address-literal ) "]"
                      ; See Section 4.1.3
     IPv4-address-literal  = Snum 3("."  Snum)
     IPv6-address-literal  = "IPv6:" IPv6-addr
     General-address-literal  = Standardized-tag ":" 1*dcontent
     Standardized-tag  = Ldh-str
                       ; Standardized-tag MUST be specified in a
                       ; Standards-Track RFC and registered with IANA
     dcontent       = %d33-90 / ; Printable US-ASCII
                    %d94-126 ; excl. "[", BACKSLASH, "]"
     Snum           = 1*3DIGIT
                    ; representing a decimal integer
                    ; value in the range 0 through 255
     IPv6-addr      = IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
     IPv6-hex       = 1*4HEXDIG
     IPv6-full      = IPv6-hex 7(":" IPv6-hex)
     IPv6-comp      = [IPv6-hex *5(":" IPv6-hex)] "::"
                    [IPv6-hex *5(":" IPv6-hex)]
                    ; The "::" represents at least 2 16-bit groups of
                    ; zeros.  No more than 6 groups in addition to the
                    ; "::" may be present.
     IPv6v4-full    = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
     IPv6v4-comp    = [IPv6-hex *3(":" IPv6-hex)] "::"
                    [IPv6-hex *3(":" IPv6-hex) ":"]
                    IPv4-address-literal
                    ; The "::" represents at least 2 16-bit groups of
                    ; zeros.  No more than 4 groups in addition to the
                    ; "::" and IPv4-address-literal may be present.

   XXX(dinosaure): about IPv4 and IPv6 parser, we use [Ipaddr].
*)

let is_dcontent = function
  | '\033' .. '\090' | '\094' .. '\126' -> true
  | _ -> false

let ipv4_address_literal =
  Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
      let raw = Bigstringaf.substring buf ~off ~len in
      let pos = ref 0 in
      try
        let res = Ipaddr.V4.of_string_raw raw pos in
        if !pos = len then Some res else None
      with Ipaddr.Parse_error _ -> None )
  >>= function Some v -> return v | None -> fail "ipv4_address_literal"

let ipv6_addr =
  Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
      let raw = Bigstringaf.substring buf ~off ~len in
      let pos = ref 0 in
      try
        let res = Ipaddr.V6.of_string_raw raw pos in
        if !pos = len then Some res else None
      with Ipaddr.Parse_error _ -> None )
  >>= function Some v -> return v | None -> fail "ipv6_addr"

let ipv6_address_literal = string "IPv6:" *> ipv6_addr

let let_dig =
  satisfy (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false)

let ldh_str =
  take_while (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
    | _ -> false )
  >>= fun ldh ->
  let_dig >>| String.make 1 >>| fun dig -> String.concat "" [ldh; dig]

let general_address_literal =
  ldh_str
  <* char ':'
  >>= fun ldh -> take_while1 is_dcontent >>| fun value -> Ext (ldh, value)

(* From RFC 5321

   Let-dig        = ALPHA / DIGIT
   Ldh-str        = *( ALPHA / DIGIT / "-" ) Let-dig
   address-literal  = "[" ( IPv4-address-literal /
                    IPv6-address-literal /
                    General-address-literal ) "]"
                    ; See Section 4.1.3

   Sometimes a host is not known to the domain name system and
   communication (and, in particular, communication to report and repair
   the error) is blocked.  To bypass this barrier, a special literal
   form of the address is allowed as an alternative to a domain name.
   For IPv4 addresses, this form uses four small decimal integers
   separated by dots and enclosed by brackets such as [123.255.37.2],
   which indicates an (IPv4) Internet Address in sequence-of-octets
   form.  For IPv6 and other forms of addressing that might eventually
   be standardized, the form consists of a standardized "tag" that
   identifies the address syntax, a colon, and the address itself, in a
   format specified as part of the relevant standards (i.e., RFC 4291
   [8] for IPv6).

   Specifically:

   IPv4-address-literal  = Snum 3("."  Snum)

   IPv6-address-literal  = "IPv6:" IPv6-addr

   General-address-literal  = Standardized-tag ":" 1*dcontent

   Standardized-tag  = Ldh-str
                     ; Standardized-tag MUST be specified in a
                     ; Standards-Track RFC and registered with IANA
   dcontent       = %d33-90 / ; Printable US-ASCII
                  %d94-126 ; excl. "[", BACKSLASH, "]"

   Snum           = 1*3DIGIT
                  ; representing a decimal integer
                  ; value in the range 0 through 255

   IPv6-addr      = IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp

   IPv6-hex       = 1*4HEXDIG

   IPv6-full      = IPv6-hex 7(":" IPv6-hex)

   IPv6-comp      = [IPv6-hex *5(":" IPv6-hex)] "::"
                  [IPv6-hex *5(":" IPv6-hex)]
                  ; The "::" represents at least 2 16-bit groups of
                  ; zeros.  No more than 6 groups in addition to the
                  ; "::" may be present.

   IPv6v4-full    = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal

   IPv6v4-comp    = [IPv6-hex *3(":" IPv6-hex)] "::"
                  [IPv6-hex *3(":" IPv6-hex) ":"]
                  IPv4-address-literal
                  ; The "::" represents at least 2 16-bit groups of
                  ; zeros.  No more than 4 groups in addition to the
                  ; "::" and IPv4-address-literal may be present.
*)
let address_literal =
  ipv4_address_literal
  >>| (fun v -> IPv4 v)
  <|> (ipv6_address_literal >>| fun v -> IPv6 v)
  <|> general_address_literal

(* Specialization of RFC 822 with RFC 5321. *)

let domain = Rfc822.domain ~address_literal
let id_right = Rfc822.id_right ~address_literal
let msg_id = Rfc822.msg_id ~address_literal
