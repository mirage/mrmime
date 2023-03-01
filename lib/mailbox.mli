(*
 * Copyright (c) 2018-2019 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t = Emile.mailbox
(** Type of mailbox. Normally, a mailbox is composed of two parts:

    {ul
    {- an optional display name that indicates the name of the recipient (which
   can be a person or a system) that could be displayed to the user of a mail
   application, and}
    {- an {i addr-spec} address enclosed in angle brackets (["<"] and [">"]).}}

    There is an alternate simple form of a mailbox where the {i addr-spec} address
   appears alone, without the recipient's name or the angle brackets. *)

(** {2 Equals.} *)

val equal : ?case_sensitive:bool -> t -> t -> bool

val escape_string : string -> string
(** [escape_string x] returns a safe [string] where control characters are
   escaped ([\x00], [\x5c], [\a], [\b], [\t], [\n], [\v], [\f], [\r] and [\x22] -
   double quote). *)

module Phrase : sig
  (** A phrase in the context of the mailbox is a {i display-name} that
     indicates the name of the recipient. We provide an easily way to make it
     and keep conformances according standards.

     {[
       let name = Phrase.(v [ w "Daniel"; w "B" ]) ;;
       val name : phrase
       Phrase.to_string name ;;
       - : string = "Daniel B"
     ]}

     About special encoding/{i charset}, a [`Word] can store a valid UTF-8 string.
     Otherwise, you can use an {!Encoded_word.t} to keep special characters:

     {[
       let name = Phrase.(v [ w "Daniel"; e ~encoding:q "Bünzli" ]) ;;
       - name : phrase
       Phrase.to_string name ;;
       - "Daniel =?UTF-8?Q?B=C3=BCnzli?=" : string
     ]}

     NOTE: we accept only valid UTF-8 contents. Other {i charset} (like [latin1])
     are NOT allowed - we can compute them but we choose to never produce them.
  *)

  type elt = [ `Dot | `Word of Emile.word | `Encoded of string * Emile.raw ]

  type 'a t =
    | [] : Peano.z t
    | ( :: ) : elt * 'a t -> 'a Peano.s t
        (** Phrase, according RFC 5322, is a non-empty list of three
      elements ({!elt}):

      {ul
      {- [`Dot] a dot surrounded by space}
      {- [`Word w] a {!word}}
      {- [`Encoded e] an encoded-word}} *)

  val o : elt
  (** {!o} produces a [`Dot]. *)

  val w : string -> elt
  (** {!w} produces a {b safe} word. {!w} will try to escape control characters
     and verify if contents respects standards. Otherwise, {!w} raises an
     [Invalid_argument]. [`Word] produced by {!w} can be surrounded by
     double-quote. *)

  val e : encoding:Encoded_word.encoding -> string -> elt
  (** {!e} is an alias of {!Encoded_word.make_exn}. About [`Encoded] word, user
     can choose the way to encode the word. {!b} for a Base64 encoding or {!q}
     for a Quoted-Printable encoding. Both accept {b only} UTF-8 contents - we
     don't accept any other {i charset}. *)

  val q : Encoded_word.encoding
  (** Quoted-Printable encoding. *)

  val b : Encoded_word.encoding
  (** Base64 encoding. *)

  val word : string -> (elt, [> `Msg of string ]) result
  (** [word x] tries to normalize [x] as a [`Word] according RFC 5322. It
     returns [Error] if [x] does not respect standards. If contents is an UTF-8
     contents, [word] will surround [x] with double-quote and will escape
     control characters (see {!escape_string}).

      NOTE: UTF-8 is allowed in e-mails according RFC 6532. *)

  val word_exn : string -> elt
  (** Same as {!word} but raises [Invalid_argument] instead to return [Error]. *)

  val coerce : 'a Peano.s t -> Emile.phrase
  (** [coerce l] returns a valid and safe {!phrase}. *)

  val make : 'a t -> (Emile.phrase, [> `Msg of string ]) result
  (** [make l] returns a {!phrase} only if [l] is a non-empty list. *)

  val v : 'a t -> Emile.phrase
  (** Same as {!make} but raises an exception instead to return [Error]. *)

  val to_string : Emile.phrase -> string
  (** [to_string x] returns a string which represents [x] as is it in a e-mails. *)
end

module Literal_domain : sig
  type 'a t
  (** A literal-domain type. Sometimes a host is not known to the domain name system
     and communication (and, in particular, communication to report and repair the
     error) is blocked. To bypass this barrier, a special literal form of the address
     is allowed as an alternative to a domain name.

     We have three kinds of literal-domain:

     {ul
     {- an IPv4 address}
     {- an IPv6 address (textual form follows RFC 4291)}
     {- an user-defined literal domain}} *)

  val ipv4 : Ipaddr.V4.t t
  (** IPv4 kind. *)

  val ipv6 : Ipaddr.V6.t t
  (** IPv6 kind. *)

  val extension : (string * string) t
  (** An user-defined literal domain kind. *)

  val make : 'a t -> 'a -> (Emile.addr, [> `Msg of string ]) result
  (** [make kind v] returns a literal-domain according RFC 5321. It should fails
     if [kind] is {!extension} and value does not respect standards. *)

  val v : 'a t -> 'a -> Emile.addr
  (** Same as {!make} but raises an exception instead to return [Error]. *)
end

module Domain : sig
  (** A domain can be constructed in several ways. The most common way is:

      {[
        let isomorphism = Domain.(v domain [ a "isomorphis"; a "me" ]) ;;
        val isomorphism : domain = `Domain ["isomorphis"; "me"]
        Domain.to_string isomorphism ;;
        - : string = "isomorphis.me"
      ]}

      You can specify an IP (v4 or v6) address like:

      {[
        let localhost = Domain.(v ipv4 Ipaddr.V4.localhost) ;;
        val localhost : domain = `Addr (IPv4 127.0.0.1)
        Domain.to_string localhost ;;
        - : string = "[127.0.0.1]"
      ]}

      At the end, and according RFC 5322, it's possible to specify a
      [`Literal] domain like this:

      {[
        let x25519 = Domain.(v literal "x25519") ;;
        val x25519 : domain = `Literal "x25519"
        Domain.to_string x25519 ;;
        - : string = "[x25519]"
      ]}

      However, this last kind conforms only RFC 5322 - RFC 5321 (SMTP protocol)
      does not recognize this kind of domain. *)

  type atom = [ `Atom of string ]
  type literal = [ `Literal of string ]

  type 'a domain =
    | ( :: ) : atom * 'a domain -> 'a Peano.s domain
    | [] : Peano.z domain

  type 'a t
  (** Kind of domain. RFC 5322 and RFC 5321 allows several kinds of domain:

      {ul
      {- An usual {!domain} which is a non-empty list of {!atom} elements}
      {- A {!Literal_domain.t}}
      {- A [`Literal] domain which is a string surrounded by brackets.}} *)

  val atom : string -> (atom, [> `Msg of string ]) result
  (** [atom x] returns a safe {!atom} element. If [x] does not respect RFC 5322,
     it returns [Error]. It accepts any characters excepts controls, space and
     specials characters - for instance, brackets are not allowed. *)

  val atom_exn : string -> atom
  (** Same as {!atom} but raises an [Invalid_argument] instead [Error]. *)

  val a : string -> atom
  (** Alias of {!atom_exn}. *)

  val literal : string -> (literal, [> `Msg of string ]) result
  (** [literal x] returns a {!literal} domain. If [x] does not respect RFC 5321,
     it returns [Error]. It will try to escape control characters
     (with {!escape_string}). *)

  val literal_exn : string -> literal
  (** Same as {!literal} but raises an [Invalid_argument] instead to return [Error]. *)

  val domain : 'a domain t
  (** Kind of domain. *)

  val ipv4 : Ipaddr.V4.t t
  (** Kind of {!Literal_domain.ipv4}. *)

  val ipv6 : Ipaddr.V6.t t
  (** Kind of {!Literal_domain.ipv6}. *)

  val extension : (string * string) t
  (** Kind of {!Literal_domain.extension}. *)

  val default : string t
  (** Kind of {!literal}. *)

  val make : 'a t -> 'a -> (Emile.domain, [> `Msg of string ]) result
  (** [make kind v] returns a safe domain. It can fail if an user-defined
     literal-domain ({!Literal_domain.extension}), a {!literal} domain or a
     {!domain} don't follow standards:

     {ul
     {- for a {!Literal_domain.extension}, [make] returns [Error] if
     {!Literal_domain.make} returns [Error]}
     {- for a {!literal}, [make] returns [Error] if {!literal} returns [Error]}
     {- for a {!domain}, [make] returns [Error] if list of {!atom} is empty}} *)

  val of_list : string list -> (Emile.domain, [> `Msg of string ]) result
  (** [of_list l] returns a domain from a non-empty list of well-formed atom
     elements. Otherwise, it returns an error. *)

  val v : 'a t -> 'a -> Emile.domain
  (** Same as {!make} but raises an [Invalid_argument] instead [Error]. *)

  val to_string : Emile.domain -> string
  (** [to_string x] returns a string which represents [x] as is it in a e-mails. *)
end

module Local : sig
  (** Local part of a mailbox is a non-empty list of {!word} elements.
      You can construct local-part like this:

      {[
        let local = Local.(v [ w "romain"; w "calascibetta" ]) ;;
        val local : local = [ `Atom "romain"; `Atom "calascibetta" ]
      ]}

      Spaces and control characters are allowed in the local-part:

      {[
        let local = Local.(v [ w "Romain Calascibetta"; w "to+mrmime" ]) ;;
        val local : local = [ `String "Romain Calascibetta"; `Atom "to+mrmime" ]
        Local.to_string local ;;
        - : string = "\"Romain Calascibetta\".to+mrmime"
      ]}

      NOTE: [+] permits your MUA to tag e-mails received with this local-part.

      Valid UTF-8 string is allowed according RFC 6532 - and will be
      surrounded by double-quote. *)

  type 'a local =
    | [] : Peano.z local
    | ( :: ) : Emile.word * 'a local -> 'a Peano.s local

  val w : string -> Emile.word
  (** {!w} produces a {b safe} word. {!w} will try to escape control characters
     and verify if contents respects standards. Otherwise, {!w} raises an
     [Invalid_argument]. [`Word] produced by {!w} can be surrounded by
     double-quote. *)

  val word : string -> (Emile.word, [> `Msg of string ]) result
  (** [word x] tries to normalize [x] as a [`Word] according RFC 5322. It
     returns [Error] if [x] does not respect standards. If contents is an UTF-8
     contents, [word] will surround [x] with double-quote and will escape
     control characters (see {!escape_string}).

      NOTE: UTF-8 is allowed in e-mails according RFC 6532. *)

  val word_exn : string -> Emile.word
  (** Same as {!word} but raises an exception instead to return [Error]. *)

  val coerce : 'a Peano.s local -> Emile.local
  (** [coerce l] returns a valid and safe {!local}. *)

  val make : 'a local -> (Emile.local, [> `Msg of string ]) result
  (** [make l] returns a {!local} only if [l] is a non-empty list. *)

  val of_list : string list -> (Emile.local, [> `Msg of string ]) result
  (** [of_list l] returns a local-part from a non-empty list of well-formed
     words. Otherwise, it returns an error. *)

  val v : 'a local -> Emile.local
  (** Same as {!make} but raises an exception instead to return [Error]. *)

  val to_string : Emile.local -> string
  (** [to_string x] returns a string which represents [x] as it is in an e-mail. *)
end

val make :
  ?name:Emile.phrase ->
  Emile.local ->
  ?domains:Emile.domain list ->
  Emile.domain ->
  t
(** [make ?name local ?domains domain] returns a {!mailbox} with local-part [local],
    first domain [domain], others domains [domains] (default is an empty list) and
    an optional name.

    {[
      let me =
        make Local.(v [ w "romain"; w "calascibetta" ])
          ~domains:[ Domain.(v domain [ a "gmail"; a "com" ]) ]
          Domain.(v domain [ a "x25519"; a "net" ]) ;;
      val me : t = ....
      to_string me ;;
      - : string = "<@gmail.com:romain.calascibetta@x25519.net>"
    ]}*)

val ( @ ) : 'a Local.local -> 'b Domain.t * 'b -> t
(** [@] operator constructs an usual e-mail address:

    {[
      let me = Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "x25519"; a "net" ]) ;;
      val me : t = { name= None
                   ; local= [ `Atom "romain"; `Atom "calascibetta" ]
                   ; domain= (`Domain ["x25519"; "net"], []) }
      to_string me ;;
      - : string = "romain.calascibetta@x25519.net"
    ]}

    With only one domain and without a {i display-name}. If you want to put
    multiple domains, you should use {!make} instead. If you want to put a {!phrase},
    you can use {!with_phrase}.

    [@] operator can raise [Invalid_argument] where local-part or domain fail to
    normalize inputs according standards (see {!Local.make} and {!Domain.make}). *)

val with_name : Emile.phrase -> t -> t
(** [with_name phrase t] put or replace {i display name} of mailbox [t].

     {[
       let dbuenzli = with_name Phrase.(v [ w "Daniel"; e ~encoding:q "Bünzli" ]) dbuenzli
       val dbuenzli : t = ...
       (* Stringify dbuenzli! *)
       to_string dbuenzli ;;
       - : string = "Daniel =?UTF-8?Q?B=C3=BCnzli?= <daniel.buenzli@erratique.ch>"
     ]}
*)

val to_string : t -> string
(** [to_string x] returns a string which represents [x] as is it in a e-mails. *)

val of_string : string -> (t, [> `Msg of string ]) result
(** [of_string x] returns a {!t} from a well-formed string [x] according RFC
   5322. A {i mailbox} can have several forms and can include [FWS] tokens. Some
   examples of what is allowed:

   {ul
   {- [thomas@gazagnaire.org]}
   {- [Hannesm <hannes@menhert.org>]}
   {- [<anil@recoil.org>]}
   {- [Romain Calascibetta <@gmail.com:romain.calascibetta@x25519.net>]}
   {- [Daniel =?UTF-8?Q?B=C3=BCnzli?= <daniel.buenzli@erratique.ch>]}}

   Any {i encoded-word} are normalized to a valid UTF-8 string (even if {i
   charset} is something else than ["UTF-8"]). *)

(** {2 Pretty-printers.} *)

val pp : Format.formatter -> t -> unit

(** {2 Decoder of mailbox.} *)

module Decoder : sig
  val mailbox : Emile.mailbox Angstrom.t
  val mailbox_list : Emile.mailbox list Angstrom.t
end

(** {2 Encoder of mailbox.} *)

module Encoder : sig
  val word : Emile.word Prettym.t
  val phrase : Emile.phrase Prettym.t
  val local : Emile.local Prettym.t
  val mailbox : t Prettym.t
  val mailboxes : t list Prettym.t
end
