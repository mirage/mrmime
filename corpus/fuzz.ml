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

let ( <.> ) f g = fun x -> f (g x)
let identity x = x

module Make (Fuzz : S) = struct
  open Fuzz
  open Mrmime

  let field_name =
    map [ string ] @@ fun v ->
    match Field_name.of_string v with Ok v -> v | Error _ -> bad_test ()

  let field_name =
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

  let date =
    map [ float; zone ] @@ fun v zone ->
    match Ptime.of_float_s v with
    | None -> bad_test ()
    | Some ptime -> Date.of_ptime ~zone ptime

  let char_from_alphabet alphabet =
    map [ range (String.length alphabet) ] (String.make 1 <.> String.get alphabet)

  let string_from_alphabet alphabet len =
    let rec go acc = function
      | 0 -> concat ~sep:(const "") acc
      | n -> go (char_from_alphabet alphabet :: acc) (pred n) in
    go [] len

  let alphabet_from_predicate predicate =
    let len = ref 0 in
    for i = 0 to 255 do if predicate (Char.chr i) then incr len done ;
    let res = Bytes.create !len in
    let pos = ref 0 in
    for i = 0 to 255 do if predicate (Char.chr i)
      then ( Bytes.set res !pos (Char.chr i) ; incr pos ) done ;
    Bytes.unsafe_to_string res

  let local =
    let atext = alphabet_from_predicate Emile.Parser.is_atext in
    let word = map [ range ~min:1 78 >>= string_from_alphabet atext ] @@ fun str ->
      match Mrmime.Mailbox.Local.word str with
      | Ok str -> str | Error _ -> bad_test () in
    list1 word

  let phrase =
    let word = map [ range ~min:1 78 >>= fixed ] @@ fun str ->
      match Mrmime.Mailbox.Phrase.word str with
      | Ok elt -> elt | Error _ -> bad_test () in
    map [ word; list (choose [ const `Dot; word ]) ] @@ List.cons

  let domain =
    let dtext = alphabet_from_predicate Emile.Parser.is_dtext in
    let word = map [ range ~min:1 78 >>= string_from_alphabet dtext ] identity in
    map [ list1 word ] @@ fun lst -> match Mailbox.Domain.of_list lst with
    | Ok v -> v | Error _ -> bad_test ()

  let ipv4 =
    map [ int; int; int; int ] @@ fun a b c d ->
    let v = Ipaddr.V4.make a b c d in
    Mailbox.Domain.v Mailbox.Domain.ipv4 v

  let ipv6 =
    map [ int; int; int; int; int; int; int; int ] @@ fun a b c d e f g h ->
    let v = Ipaddr.V6.make a b c d e f g h in
    Mailbox.Domain.v Mailbox.Domain.ipv6 v

  let domain = choose [ domain; ipv4; ipv6 ]

  let mailbox =
    map [ option phrase; local; list1 domain ] @@ fun name local vs ->
    match vs with
    | hd :: tl -> Emile.{ name; local; domain= (hd, tl) }
    | [] -> bad_test ()

  let mailboxes = list1 mailbox

  let group : Emile.t t = map [ phrase; list1 mailbox ] @@ fun name mailboxes ->
    match Group.make ~name mailboxes with
    | Some v -> (`Group v :> Emile.t) | None -> bad_test ()

  let address : Emile.t t =
    let mailbox : Emile.t t = map [ mailbox ] @@ fun v -> (`Mailbox v :> Emile.t) in
    choose [ group; mailbox ]

  let addresses : Emile.t List.t t = list1 address

  let token = alphabet_from_predicate Mrmime.Content_type.is_token
  let token = range ~min:1 32 >>= string_from_alphabet token

  let x_token =
    map [ choose [ const "x"; const "X" ]; token ] @@ fun x token ->
    x ^ "-" ^ token

  let ty =
    choose
      [ const `Text
      ; const `Image
      ; const `Audio
      ; const `Video
      ; const `Application
      ; const `Message
      ; const `Multipart
      ; map [ x_token ] @@ fun v -> `X_token v ]

  let subty ty =
    choose
      (Mrmime.Iana.Map.find
          (Mrmime.Content_type.Type.to_string ty)
          Mrmime.Iana.database
       |> Mrmime.Iana.Set.elements
       |> List.map const)

  let subty ty = map [ subty ty ] @@ fun subty ->
    ty, Content_type.Subtype.v ty subty

  let key = map [ token ] @@ fun v ->
    match Mrmime.Content_type.Parameters.key v with
    | Ok v -> v | Error _ -> bad_test ()

  let token =
    alphabet_from_predicate @@ fun chr ->
    Content_type.is_token chr || Content_type.is_qtext chr

  let token = range ~min:1 32 >>= string_from_alphabet token

  let value =
    map [ token ] @@ fun v ->
    match Content_type.Parameters.value v with
    | Ok v -> v | Error _ -> bad_test ()

  let parameters =
    map [ list (pair key value) ] Content_type.Parameters.of_list

  let content_type =
    map [ ty >>= subty; parameters ] @@ fun (ty, subty) parameters ->
    Content_type.make ty subty parameters

  let content_encoding : Content_encoding.t t =
    choose
      [ const `Base64
      ; const `Bit8
      ; const `Bit7
      ; const `Binary
      ; const `Quoted_printable
      ; map [ x_token ] (fun v -> `X_token v) ]

  let value =
    let date = map [ date ] @@ fun v -> `Date v in
    let mailboxes = map [ list1 mailbox ] @@ fun v -> `Mailboxes v in
    let mailbox = map [ mailbox ] @@ fun v -> `Mailbox v in
    let addresses = map [ list1 address ] @@ fun v -> `Addresses v in
    let phrases = map [ list1 phrase ] @@ fun v -> `Phrases v in
    let content = map [ content_type ] @@ fun v -> `Content v in
    let encoding = map [ content_encoding ] @@ fun v -> `Encoding v in
    choose [ date; mailboxes; mailbox; addresses; phrases; content; encoding; ]

  let field = map [ field_name; value ] @@ fun field_name -> function
    | `Date v -> Field.Field (field_name, Field.Date, v)
    | `Mailboxes v -> Field.Field (field_name, Field.Mailboxes, v)
    | `Mailbox v -> Field.Field (field_name, Field.Mailbox, v)
    | `Addresses v -> Field.Field (field_name, Field.Addresses, v)
    | `Phrases v -> Field.Field (field_name, Field.Phrases, v)
    | `Content v -> Field.Field (field_name, Field.Content, v)
    | `Encoding v -> Field.Field (field_name, Field.Encoding, v)

  let header = map [ list1 field ] @@ Header.of_list
end
