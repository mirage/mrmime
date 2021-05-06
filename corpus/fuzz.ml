(** Fuzzer monad  *)
module type S = sig
  type 'a t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val char : char t
  val float : float t
  val string : string t
  val bool : bool t
  val int : int t
  val range : ?min:int -> int -> int t
  val choose : 'a t list -> 'a t
  val const : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val concat : sep:string t -> string t list -> string t
  val list : 'a t -> 'a list t
  val list1 : 'a t -> 'a list t
  val fixed : int -> string t
  val option : 'a t -> 'a option t

  type (_, _) list =
    | [] : ('res, 'res) list
    | ( :: ) : 'a t * ('k, 'res) list -> ('a -> 'k, 'res) list

  val map : ('f, 'a) list -> 'f -> 'a t
  val bad_test : unit -> 'a
end

let ( <.> ) f g x = f (g x)
let identity x = x

module Make (Fuzz : S) = struct
  open Fuzz
  open Mrmime

  let char_from_alphabet alphabet =
    map
      [ range (String.length alphabet) ]
      (String.make 1 <.> String.get alphabet)

  let string_from_alphabet alphabet len =
    let rec go acc = function
      | 0 -> concat ~sep:(const "") acc
      | n -> go (char_from_alphabet alphabet :: acc) (pred n)
    in
    go [] len

  let alphabet_from_predicate predicate =
    let len = ref 0 in
    for i = 0 to 255 do
      if predicate (Char.chr i) then incr len
    done;
    let res = Bytes.create !len in
    let pos = ref 0 in
    for i = 0 to 255 do
      if predicate (Char.chr i) then (
        Bytes.set res !pos (Char.chr i);
        incr pos)
    done;
    Bytes.unsafe_to_string res

  (** Header generator: an Header is a pair of a name and a value.  *)

  (** [field_name] generates a field name (either a random string or a
     prefined field name). *)
  let field_name =
    (* generate a random field name *)
    let field_name =
      map [ string ] @@ fun v ->
      match Field_name.of_string v with Ok v -> v | Error _ -> bad_test ()
    in
    (* the field name is either in a predefined list of name ("Date",
       "From", "Sender" etc .. or a random one *)
    choose
      [
        const Field_name.date; const Field_name.from; const Field_name.sender;
        const Field_name.reply_to; const Field_name.cc; const Field_name.bcc;
        const Field_name.subject; const Field_name.message_id;
        const Field_name.in_reply_to; const Field_name.references;
        const Field_name.comments; const Field_name.keywords;
        const Field_name.received; const Field_name.return_path;
        const Field_name.content_type; const Field_name.content_encoding;
        const Field_name.mime_version; const Field_name.content_id;
        const Field_name.content_description; const Field_name.resent_date;
        const Field_name.resent_from; const Field_name.resent_sender;
        const Field_name.resent_to; const Field_name.resent_cc;
        const Field_name.resent_bcc; const Field_name.resent_message_id;
        const Field_name.resent_reply_to; field_name;
      ]

  (** As mrmime parses the value of some headers, the generator must
     be able to generate these values properly. These values have types :
     {ul
     {- [Date.t]}
     {- [Maibox.t] and [Mailbox.t list]}
     {- [Address.t]}
     {- [MessageID.t]}
     {- [Emile.phrase list]}
     {- [Content_type.t]}
     {- [Content_encoding.t]}}

     The following generators are for values of all those types. *)
  let zone =
    choose
      [
        const Date.Zone.UT; const Date.Zone.GMT; const Date.Zone.EST;
        const Date.Zone.EDT; const Date.Zone.CST; const Date.Zone.CDT;
        const Date.Zone.MST; const Date.Zone.MDT; const Date.Zone.PST;
        const Date.Zone.PDT;
        map [ char ] (fun chr ->
            match Date.Zone.military_zone chr with
            | Ok v -> v
            | Error _ -> bad_test ());
        ( map [ range 24; range 60; bool ] @@ fun mm hh -> function
          | true -> Date.Zone.TZ (hh, mm) | false -> Date.Zone.TZ (-hh, mm) );
      ]

  (** Date *)
  let date : Date.t t =
    map [ float; zone ] @@ fun v zone ->
    match Ptime.of_float_s v with
    | None -> bad_test ()
    | Some ptime -> Date.of_ptime ~zone ptime

  (** Mailbox : a maibox is composed of three parts: a phrase (optional display name),
       a local part (username) and a domain part. *)
  let local =
    let atext = alphabet_from_predicate Emile.Parser.is_atext in
    let word =
      map [ range ~min:1 78 >>= string_from_alphabet atext ] @@ fun str ->
      match Mailbox.Local.word str with Ok str -> str | Error _ -> bad_test ()
    in
    list1 word

  let phrase =
    let word =
      map [ range ~min:1 78 >>= fixed ] @@ fun str ->
      match Mailbox.Phrase.word str with
      | Ok elt -> elt
      | Error _ -> bad_test ()
    in
    map [ word; list (choose [ const `Dot; word ]) ] @@ List.cons

  let domain =
    let domain =
      let dtext = alphabet_from_predicate Emile.Parser.is_dtext in
      let word = range ~min:1 78 >>= string_from_alphabet dtext in
      map [ list1 word ] @@ fun lst ->
      match Mailbox.Domain.of_list lst with Ok v -> v | Error _ -> bad_test ()
    in
    let ipv4 =
      map [ int; int; int; int ] @@ fun a b c d ->
      let v = Ipaddr.V4.make a b c d in
      Mailbox.Domain.v Mailbox.Domain.ipv4 v
    in
    let ipv6 =
      map [ int; int; int; int; int; int; int; int ] @@ fun a b c d e f g h ->
      let v = Ipaddr.V6.make a b c d e f g h in
      Mailbox.Domain.v Mailbox.Domain.ipv6 v
    in
    choose [ domain; ipv4; ipv6 ]

  let mailbox : Mailbox.t t =
    map [ option phrase; local; list1 domain ] @@ fun name local vs ->
    match vs with
    | hd :: tl -> Emile.{ name; local; domain = (hd, tl) }
    | [] -> bad_test ()

  let mailboxes : Mailbox.t List.t t = list1 mailbox

  (** Address *)
  let address : Address.t t =
    let group =
      map [ phrase; list1 mailbox ] @@ fun name mailboxes ->
      match Group.make ~name mailboxes with
      | Some v -> Address.group v
      | None -> bad_test ()
    in
    let mailbox : Emile.t t = map [ mailbox ] @@ fun v -> Address.mailbox v in
    choose [ group; mailbox ]

  (*let addresses : Address.t List.t t = list1 address*)

  (** Content-encoding: [ty/subty;q=value] *)
  let token =
    let alphabet = alphabet_from_predicate Mrmime.Content_type.is_token in
    range ~min:1 32 >>= string_from_alphabet alphabet

  let x_token =
    map [ choose [ const "x"; const "X" ]; token ] @@ fun x token ->
    x ^ "-" ^ token

  let ty : Content_type.Type.t t =
    choose
      [
        const `Text; const `Image; const `Audio; const `Video;
        const `Application; const `Message; const `Multipart;
        (map [ x_token ] @@ fun v -> `X_token v);
      ]

  let subty ty =
    let possible_subty =
      Iana.Map.find (Content_type.Type.to_string ty) Iana.database
      |> Iana.Set.elements
    in
    map [ choose (List.map const possible_subty) ] @@ fun subty ->
    (ty, Content_type.Subtype.v ty subty)

  let parameters =
    let key =
      map [ token ] @@ fun v ->
      match Content_type.Parameters.key v with
      | Ok v -> v
      | Error _ -> bad_test ()
    in
    let value =
      let abc =
        alphabet_from_predicate @@ fun chr ->
        Content_type.is_token chr || Content_type.is_qtext chr
      in
      let token = range ~min:1 32 >>= string_from_alphabet abc in
      map [ token ] @@ fun v ->
      match Content_type.Parameters.value v with
      | Ok v -> v
      | Error _ -> bad_test ()
    in
    map [ list (pair key value) ] Content_type.Parameters.of_list

  let content_type =
    map [ ty >>= subty; parameters ] @@ fun (ty, subty) parameters ->
    Content_type.make ty subty parameters

  (** Content-encoding : `Bit7, `Bit8, `Binary, `Quoted_printable,
     `Base64, `X_token of string and `Ietf_token.

     Ietf_token is not parsed (assert false) so we avoid generating such
     an encoding. *)
  let content_encoding : Content_encoding.t t =
    choose
      [
        const `Bit7; const `Bit8; const `Binary; const `Quoted_printable;
        const `Base64; map [ x_token ] (fun v -> `X_token v);
      ]

end
