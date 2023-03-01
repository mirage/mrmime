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

type t = Emile.group

module Phrase = Mailbox.Phrase

let equal = Emile.equal_group

let make ~name:group mailboxes =
  if List.length mailboxes = 0 then None else Some { Emile.group; mailboxes }

let v ~name mailboxes =
  match make ~name mailboxes with
  | None -> invalid_arg "A group contains at least one mailbox"
  | Some t -> t

let pp = Emile.pp_group

module Decoder = struct
  let group = Emile.Parser.group
end

module Encoder = struct
  open Prettym

  let comma = ((fun ppf () -> eval ppf [ char $ ','; fws ]), ())
  let phrase = Mailbox.Encoder.phrase
  let mailbox = Mailbox.Encoder.mailbox

  let group ppf t =
    eval ppf
      [ box;
        !!phrase;
        char $ ':';
        spaces 1;
        box;
        !!(list ~sep:comma mailbox);
        close;
        char $ ';';
        close
      ]
      t.Emile.group t.Emile.mailboxes
end
