type word = Rfc822.word
type phrase = Rfc5322.phrase
type literal_domain = Rfc5321.literal_domain
type domain = Rfc5322.domain
type local = Rfc822.local

type t = Rfc5322.mailbox =
  {name: phrase option; local: local; domain: domain * domain list}

val equal_word : ?sensitive:bool -> word -> word -> bool
val equal_phrase : phrase -> phrase -> bool
val equal_local : local -> local -> bool
val equal_domain : domain -> domain -> bool
val equal_literal_domain : literal_domain -> literal_domain -> bool
val equal : t -> t -> bool

module Peano : sig
  type z = Z
 and 'a s = S
end

module Phrase : sig
  type elt = [ `Dot | `Word of word | `Encoded of Encoded_word.t ]
  type 'a t = [] : Peano.z t | ( :: ) : elt * 'a t -> 'a Peano.s t

  val o : elt

  (* / *)

  val word : string -> elt option
  val word_exn : string -> elt
  val w : string -> elt

  (* / *)

  val e : encoding:Encoded_word.encoding -> string -> elt

  (* / *)

  val coerce : 'a Peano.s t -> phrase
  val make : 'a t -> phrase option
  val make_exn : 'a t -> phrase
end

module Literal_domain : sig
  type 'a t

  val ipv4 : Ipaddr.V4.t t
  val ipv6 : Ipaddr.V6.t t
  val extension : (string * string) t

  (* / *)

  val make : 'a t -> 'a -> literal_domain option
  val make_exn : 'a t -> 'a -> literal_domain
end

module Domain : sig
  type atom = [`Atom of string]
  type literal = [`Literal of string]

  type 'a domain =
    | ( :: ) : atom * 'a domain -> 'a Peano.s domain
    | [] : Peano.z domain

  type 'a t

  val atom : string -> atom option
  val atom_exn : string -> atom
  val a : string -> atom

  (* / *)

  val literal : string -> literal option
  val literal_exn : string -> literal

  (* / *)

  val domain : 'a domain t
  val ipv4 : Ipaddr.V4.t t
  val ipv6 : Ipaddr.V6.t t
  val extension : (string * string) t
  val default : literal t

  (* / *)

  val make : 'a t -> 'a -> Rfc5322.domain option
  val make_exn : 'a t -> 'a -> Rfc5322.domain
end

module Local : sig
  type 'a local =
    | [] : Peano.z local
    | ( :: ) : word * 'a local -> 'a Peano.s local

  val word : string -> word option
  val word_exn : string -> word
  val w : string -> word

  (* / *)

  val coerce : 'a Peano.s local -> Rfc822.local
  val make : 'a local -> Rfc822.local option
  val make_exn : 'a local -> Rfc822.local
end

val ( @ ) : 'a Local.local -> 'b Domain.t * 'b -> t option
val with_name : phrase -> t -> t

(* / *)

val pp_phrase : phrase Fmt.t
val pp_word : word Fmt.t
val pp_literal_domain : literal_domain Fmt.t
val pp_domain : domain Fmt.t
val pp_local : local Fmt.t
val pp : t Fmt.t

module Encoder : sig
  val mailbox : t Encoder.t
end
