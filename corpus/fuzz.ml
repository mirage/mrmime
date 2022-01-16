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
  val fix : ('a t -> 'a t) -> 'a t

  type (_, _) gens =
    | [] : ('res, 'res) gens
    | ( :: ) : 'a t * ('k, 'res) gens -> ('a -> 'k, 'res) gens

  val map : ('f, 'a) gens -> 'f -> 'a t
  val bad_test : string -> 'a
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
      match Field_name.of_string v with
      | Ok v -> v
      | Error _ -> bad_test "field_name"
    in
    (* the field name is either in a predefined list of name ("Date",
       "From", "Sender" etc .. or a random one *)
    choose
      [ const Field_name.date;
        const Field_name.from;
        const Field_name.sender;
        const Field_name.reply_to;
        const Field_name.cc;
        const Field_name.bcc;
        const Field_name.subject;
        (*const Field_name.message_id;*)
        const Field_name.in_reply_to;
        const Field_name.references;
        const Field_name.comments;
        const Field_name.keywords;
        const Field_name.received;
        const Field_name.return_path;
        const Field_name.content_type;
        const Field_name.content_encoding;
        const Field_name.mime_version;
        const Field_name.content_id;
        const Field_name.content_description;
        const Field_name.resent_date;
        const Field_name.resent_from;
        const Field_name.resent_sender;
        const Field_name.resent_to;
        const Field_name.resent_cc;
        const Field_name.resent_bcc;
        const Field_name.resent_message_id;
        const Field_name.resent_reply_to;
        field_name
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
      Date.Zone.
        [ const UT;
          const GMT;
          const EST;
          const EDT;
          const CST;
          const CDT;
          const MST;
          const MDT;
          const PST;
          const PDT;
          map [ char ] (fun chr ->
              match Date.Zone.military_zone chr with
              | Ok v -> v
              | Error _ -> bad_test "military_zone");
          ( map [ range 24; range 60; bool ] @@ fun mm hh -> function
            | true -> Date.Zone.TZ (hh, mm) | false -> Date.Zone.TZ (-hh, mm) )
        ]

  let month =
    choose
      Date.Month.
        [ const Jan;
          const Feb;
          const Mar;
          const Apr;
          const May;
          const Jun;
          const Jul;
          const Aug;
          const Sep;
          const Oct;
          const Nov;
          const Dec
        ]

  (** Date *)
  let date =
    let year = range ~min:1000 9999 in
    let day = range ~min:1 31 in
    let hour = range ~min:0 23 in
    let minute = range ~min:0 59 in
    let second = range ~min:0 59 in
    map [ year; month; day; hour; minute; option second; zone ]
    @@ fun year month day hour minute second zone ->
    match Mrmime.Date.make (year, month, day) (hour, minute, second) zone with
    | Ok date -> date
    | _ -> bad_test "date"

  (** Mailbox : a maibox is composed of three parts: a phrase (optional display name),
       a local part (username) and a domain part. *)
  let local =
    let atext = alphabet_from_predicate Emile.Parser.is_atext in
    let word =
      map [ range ~min:1 78 >>= string_from_alphabet atext ] @@ fun str ->
      match Mailbox.Local.word str with
      | Ok str -> str
      | Error _ -> bad_test "local/word"
    in
    list1 word

  let phrase =
    let atext = alphabet_from_predicate Emile.Parser.is_atext in
    let word =
      map [ range ~min:1 78 >>= string_from_alphabet atext ] @@ fun str ->
      match Mailbox.Phrase.word str with
      | Ok elt -> elt
      | Error _ -> bad_test "phrase/word"
    in
    map [ word; list (choose [ const `Dot; word ]) ] @@ List.cons

  let domain =
    let domain =
      let dtext = alphabet_from_predicate Emile.Parser.is_dtext in
      let word = range ~min:1 78 >>= string_from_alphabet dtext in
      map [ list1 word ] @@ fun lst ->
      match Mailbox.Domain.of_list lst with
      | Ok v -> v
      | Error _ -> bad_test "domain"
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
    | [] -> bad_test "mailbox"

  let mailboxes : Mailbox.t List.t t = list1 mailbox

  (** Address *)
  let address : Address.t t =
    let group =
      map [ phrase; list1 mailbox ] @@ fun name mailboxes ->
      match Group.make ~name mailboxes with
      | Some v -> Address.group v
      | None -> bad_test "group"
    in
    let mailbox : Emile.t t = map [ mailbox ] @@ fun v -> Address.mailbox v in
    choose [ group; mailbox ]

  (** Content-type: [ty/subty;q=value] *)
  let token =
    let alphabet = alphabet_from_predicate Mrmime.Content_type.is_token in
    range ~min:1 32 >>= string_from_alphabet alphabet

  let x_token =
    map [ choose [ const "x"; const "X" ]; token ] @@ fun x token ->
    x ^ "-" ^ token

  let ty : Content_type.Type.t t =
    choose
      [ const `Text;
        const `Image;
        const `Audio;
        const `Video;
        const `Application;
        const `Message;
        const `Multipart;
        (map [ x_token ] @@ fun v -> `X_token v)
      ]

  let subty ty =
    if Content_type.Type.(is_discrete ty || is_multipart ty || is_message ty)
    then
      let possible_subty =
        Iana.Map.find (Content_type.Type.to_string ty) Iana.database
        |> Iana.Set.elements
      in
      map [ choose (List.map const possible_subty) ] @@ fun subty ->
      (ty, Content_type.Subtype.v ty subty)
    else
      map [ x_token ] @@ fun token ->
      match Content_type.Subtype.extension token with
      | Ok xtoken -> (ty, xtoken)
      | _ -> bad_test "subty/extension"

  let parameters =
    let key =
      map [ token ] @@ fun v ->
      match Content_type.Parameters.key v with
      | Ok v -> v
      | Error _ -> bad_test "content-type/key"
    in
    let token =
      let abc =
        alphabet_from_predicate @@ fun chr ->
        Content_type.is_token chr || Content_type.is_qtext chr
      in
      range ~min:1 32 >>= string_from_alphabet abc
    in
    let value =
      map [ token ] @@ fun v ->
      match Content_type.Parameters.value v with
      | Ok v -> v
      | Error _ -> bad_test "content-type/value"
    in
    map [ list (pair key value) ] Content_type.Parameters.of_list

  let content_type =
    map [ ty >>= subty; parameters ] @@ fun (ty, subty) parameters ->
    Content_type.make ty subty parameters

  (** Content-encoding : `Bit7, `Bit8, `Binary, `Quoted_printable,
     `Base64, `X_token of string and `Ietf_token.

     Ietf_token and `X_token are not by mrmime so we avoid
     generating such an encoding. *)
  let content_encoding : Content_encoding.t t =
    choose
      [ const `Base64;
        const `Bit8;
        const `Bit7;
        const `Binary;
        const `Quoted_printable
      ]

  let messageid : Mrmime.MessageID.t t =
    let dtext = alphabet_from_predicate Emile.Parser.is_dtext in
    let word = range ~min:1 78 >>= string_from_alphabet dtext in
    let domain =
      map [ list1 word ] @@ fun w ->
      let w =
        List.(
          map MessageID.Domain.atom w
          |> fold_left
               (fun acc elt ->
                 match elt with Some (`Atom x) -> x :: acc | None -> acc)
               [])
      in
      match w with [] -> bad_test "message-id" | _ -> `Domain w
    in
    let literal = map [ word ] @@ fun w -> MessageID.Domain.(v default w) in
    map [ local; choose [ domain; literal ] ] @@ fun local domain ->
    (local, domain)

  (** [field] randomly generates a single header. As some headers are
     parsed by mrmime, we make sure to generate these ones with the right
     value. *)
  let field =
    (* Grammar of unstructured header field follows two rfcs:
        - rfc5322 :
          unstructured    =   (*([FWS] VCHAR)*)
        - rfc5234 :
          VCHAR = %x21-7E *)
    let unstructured : Unstructured.t t =
      let abc =
        alphabet_from_predicate (function
          | '\x21' .. '\x7e' -> true
          | _ -> false)
      in
      map [ range ~min:5 15 >>= string_from_alphabet abc ] @@ fun s ->
      (match Unstrctrd.of_string (s ^ "\r\n") with
       | Ok (_, s) -> s
       | _ -> bad_test "Unstructured"
        :> Unstructured.t)
    in
    field_name >>= fun field_name ->
    if Field_name.(equal field_name date) then
      (* date*)
      map [ date ] @@ fun date -> Field.(Field (field_name, Date, date))
    else if Field_name.(equal field_name from) then
      (* from*)
      map [ mailboxes ] @@ fun mailboxes ->
      Field.(Field (field_name, Mailboxes, mailboxes))
    else if Field_name.(equal field_name sender) then
      (* sender *)
      map [ mailbox ] @@ fun mailbox ->
      Field.(Field (field_name, Mailbox, mailbox))
    else if Field_name.(equal field_name reply_to) then
      (* reply_to *)
      map [ list1 address ] @@ fun addresses ->
      Field.(Field (field_name, Addresses, addresses))
    else if Field_name.(equal field_name cc) then
      (* cc *)
      map [ list1 address ] @@ fun addresses ->
      Field.(Field (field_name, Addresses, addresses))
    else if Field_name.(equal field_name bcc) then
      (* bcc *)
      map [ list1 address ] @@ fun addresses ->
      Field.(Field (field_name, Addresses, addresses))
    else if Field_name.(equal field_name message_id) then
      (* message-id *)
      map [ messageid ] @@ fun message_id ->
      Field.(Field (field_name, MessageID, message_id))
    else if Field_name.(equal field_name content_type) then
      (* content-type *)
      map [ content_type ] @@ fun content_type ->
      Field.(Field (field_name, Content, content_type))
    else if Field_name.(equal field_name content_encoding) then
      (* content-transfer-encoding *)
      map [ content_encoding ] @@ fun encoding ->
      Field.(Field (field_name, Encoding, encoding))
    else
      map [ unstructured ] @@ fun unstructured ->
      Field.(Field (field_name, Unstructured, unstructured))

  let header = map [ list1 field ] @@ Header.of_list

  (** body generator *)
  let body header : string t =
    let is_bit7 = function '\000' .. '\127' -> true | _ -> false in
    let body predicat =
      let abc = alphabet_from_predicate predicat in
      range ~min:1 1000 >>= string_from_alphabet abc
    in
    match Header.content_encoding header with
    | `Bit7 | `Binary -> body is_bit7
    | `Bit8 -> body (fun _ -> true)
    | `Quoted_printable | `Base64 -> body (fun _ -> true)
    | `Ietf_token _ | `X_token _ -> assert false

  (** [header_ct ty] generates header with a given content-type type [ty]. *)
  let header_ct ty =
    map [ header; subty ty; parameters ] @@ fun h (ty, subty) param ->
    let content_type = Content_type.make ty subty param in
    Header.replace Field_name.content_type (Field.Content, content_type) h

  (** [mail] recursively generates an email.  *)
  let mail : (Header.t * string Mail.t) t =
    fix (fun mail ->
        choose
          [ const `Message;
            const `Multipart;
            const `Text;
            const `Image;
            const `Audio;
            const `Video;
            const `Application
          ]
        >>= fun ty ->
        header_ct ty >>= fun h_parent ->
        match ty with
        | #Content_type.Type.discrete ->
            map [ body h_parent ] @@ fun body -> (h_parent, Mail.Leaf body)
        (*| `Message ->
            (* mail in a mail: the mail [m] is build recursively and
               converted into a stream with [Mt.to_stream m]. This stream
               is used to build a part and then the parent mail. No new
               header are generated for the [Mt.part] function as it
               would be concatenated with [h] by the [Mt.make]
               function. *)
            map [ mail ] @@ fun (h, b) -> (h_parent, Mail.(Message (h, b)))*)
        | `Multipart ->
            let parts : (Header.t * string Mail.t option) list t =
              map [ list1 (pair header (option mail)) ] @@ fun parts ->
              List.map
                (function
                  | h, None -> (h, None) | _, Some (h, m) -> (h, Some m))
                parts
            in
            map [ parts ] @@ fun parts -> (h_parent, Mail.Multipart parts)
        | _ -> bad_test "mail/content_type")
end
