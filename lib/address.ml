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

type t = Emile.t

let group group = `Group group
let mailbox mailbox = `Mailbox mailbox

let equal a b =
  match (a, b) with
  | `Group a, `Group b -> Group.equal a b
  | `Mailbox a, `Mailbox b -> Mailbox.equal a b
  | _ -> false

let pp = Emile.pp

module Decoder = struct
  let address = Emile.Parser.address
  let address_list = Emile.Parser.address_list
end

module Encoder = struct
  open Prettym

  let mailbox = Mailbox.Encoder.mailbox
  let group = Group.Encoder.group
  let comma = ((fun ppf () -> eval ppf [ char $ ','; fws ]), ())

  let address ppf = function
    | `Mailbox m -> mailbox ppf m
    | `Group g -> group ppf g

  let addresses ppf l =
    eval ppf [ tbox 1; !!(list ~sep:comma address); close ] l
end
