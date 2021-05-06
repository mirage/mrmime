module type S = sig
  type 'a t

  val char : char t
  val float : float t
  val string : string t
  val bool : bool t
  val range : ?min:int -> int -> int t
  val choose : 'a t list -> 'a t
  val const : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val range : ?min:int -> int -> int t
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

  let mailbox =
    map [ option phrase; local; list1 domain ] @@ fun name local vs ->
    match vs with
    | hd :: tl -> Emile.{ name; local; domain= (hd, tl) }
    | [] -> assert false
end
