module Refl = struct type ('a, 'b) t = Refl : ('a, 'a) t end

let parser =
  let open Angstrom in
  let open Rfc5322 in
  (* XXX(dinosaure): should be in [Rfc5322] module. *)
  let nothing_to_do _ = fail "Nothing to do" in
  field_name <* many (satisfy (function '\x09' | '\x20' -> true | _ -> false))
  <* char ':'
  >>= (fun field_name -> field nothing_to_do field_name)
      <|> (lines >>| fun lines -> `Lines lines)

let parser =
  let open Angstrom in
  let crlf = string "\r\n" in
  (Rfc5322.with_location parser) <|> (Rfc5322.with_location (crlf >>= fun _ -> return `End))

module Value = struct
  type phrase_or_message_id = [`Phrase of Rfc5322.phrase | `MessageID of Rfc822.nonsense Rfc822.msg_id]
  type trace = Rfc5322.mailbox option * ([ `Addr of Rfc5322.mailbox | `Domain of Rfc5322.domain | `Word of Rfc822.word ] list * Rfc5322.date option) list

  type 'a t =
    | Date : Rfc5322.date t
    | Mailboxes : Rfc5322.mailbox list t
    | Mailbox : Rfc5322.mailbox t
    | Addresses : Rfc5322.address list t
    | MessageID : Rfc822.nonsense Rfc822.msg_id t
    | Phrase_or_message_id : phrase_or_message_id list t
    | Unstructured : Rfc5322.unstructured t
    | Phrases : Rfc5322.phrase list t
    | Trace : trace t
    | Lines : string list t

  type value = V : 'a t -> value

  let of_string x = match String.lowercase_ascii x with
    | "date" -> Ok (V Date)
    | "mailboxes" -> Ok (V Mailboxes)
    | "mailbox" -> Ok (V Mailbox)
    | "addresses" -> Ok (V Addresses)
    | "message-id" -> Ok (V MessageID)
    | "phrase-or-message-id" -> Ok (V Phrase_or_message_id)
    | "unstructured" -> Ok (V Unstructured)
    | "phrases" -> Ok (V Phrases)
    | "trace" -> Ok (V Trace)
    | "lines" -> Ok (V Lines)
    | x -> Rresult.R.error_msgf "Invalid value kind: %s" x

  let pp : type a. a t Fmt.t = fun ppf -> function
      | Date -> Fmt.string ppf "Date"
      | Mailboxes -> Fmt.string ppf "Mailboxes"
      | Mailbox -> Fmt.string ppf "Mailbox"
      | Addresses -> Fmt.string ppf "Addresses"
      | MessageID -> Fmt.string ppf "Message-ID"
      | Phrase_or_message_id -> Fmt.string ppf "Phrase-or-message-id"
      | Unstructured -> Fmt.string ppf "Unstructured"
      | Phrases -> Fmt.string ppf "Phrases"
      | Trace -> Fmt.string ppf "Trace"
      | Lines -> Fmt.string ppf "Lines"

  let pp_of_value : type a. a t -> a Fmt.t = function
    | Date -> Date.pp
    | Mailboxes -> Fmt.(Dump.list Mailbox.pp)
    | Mailbox -> Mailbox.pp
    | Addresses -> Fmt.(Dump.list Address.pp)
    | MessageID -> MessageID.pp
    | Phrase_or_message_id -> assert false (* TODO *)
    | Unstructured -> Unstructured.pp
    | Phrases -> assert false (* TODO *)
    | Trace -> assert false (* TODO *)
    | Lines -> Fmt.(Dump.list string)

  type binding = B : Field.t * 'a t * 'a * Location.t -> binding
               | L : string list * Location.t -> binding

  let equal
    : type a b. a t -> b t -> (a, b) Refl.t option
    = fun a b -> match a, b with
      | Date, Date -> Some Refl.Refl
      | Mailboxes, Mailboxes -> Some Refl.Refl
      | Mailbox, Mailbox -> Some Refl.Refl
      | Addresses, Addresses -> Some Refl.Refl
      | MessageID, MessageID -> Some Refl.Refl
      | Phrase_or_message_id, Phrase_or_message_id -> Some Refl.Refl
      | Unstructured, Unstructured -> Some Refl.Refl
      | Phrases, Phrases -> Some Refl.Refl
      | Trace, Trace -> Some Refl.Refl
      | Lines, Lines -> Some Refl.Refl
      | _, _ -> None
end

module Q = Ke.Rke.Weighted
type q = (char, Bigarray.int8_unsigned_elt) Q.t

type 'value decoder =
  { q : q
  ; b : Bigstringaf.t
  ; f : Field.t
  ; v : 'value Value.t
  ; mutable cs : ([ Rfc5322.field | `End ] * Location.t) Angstrom.Unbuffered.state }

type 'value decode =
  [ `Field of 'value
  | `Other of Value.binding
  | `Await
  | `End
  | `Malformed of string ]

let to_binding
  : (Rfc5322.field * Location.t) -> Value.binding
  = fun (field, location) -> match field with
    | `Date date ->
      Value.(B (Field.of_string_exn "Date", Value.Date, date, location))
    | `From mailboxes ->
      Value.(B (Field.of_string_exn "From", Value.Mailboxes, mailboxes, location))
    | `Sender mailbox ->
      Value.(B (Field.of_string_exn "Sender", Value.Mailbox, mailbox, location))
    | `ReplyTo addresses ->
      Value.(B (Field.of_string_exn "Reply-To", Value.Addresses, addresses, location))
    | `To addresses ->
      Value.(B (Field.of_string_exn "To", Value.Addresses, addresses, location))
    | `Cc addresses ->
      Value.(B (Field.of_string_exn "Cc", Value.Addresses, addresses, location))
    | `Bcc addresses ->
      Value.(B (Field.of_string_exn "Bcc", Value.Addresses, addresses, location))
    | `MessageID message_id ->
      Value.(B (Field.of_string_exn "Message-ID", Value.MessageID, message_id, location))
    | `Subject unstructured ->
      Value.(B (Field.of_string_exn "Subject", Value.Unstructured, unstructured, location))
    | `Comments unstructured ->
      Value.(B (Field.of_string_exn "Comments", Value.Unstructured, unstructured, location))
    | `Keywords phrases ->
      Value.(B (Field.of_string_exn "Keywords", Value.Phrases, phrases, location))
    | `Field (field, unstructured) ->
      Value.(B (Field.of_string_exn field, Value.Unstructured, unstructured, location))
    | `Trace trace ->
      Value.(B (Field.of_string_exn "Return-Path", Value.Trace, trace, location))
    | `Unsafe (field, unstructured) ->
      Value.(B (Field.of_string_exn field, Value.Unstructured, unstructured, location))
    | `InReplyTo x ->
      Value.(B (Field.of_string_exn "In-Reply-To", Value.Phrase_or_message_id, x, location))
    | `References x ->
      Value.(B (Field.of_string_exn "References", Value.Phrase_or_message_id, x, location))
    | `ResentDate date ->
      Value.(B (Field.of_string_exn "Resent-Date", Value.Date, date, location))
    | `ResentFrom mailboxes ->
      Value.(B (Field.of_string_exn "Resent-From", Value.Mailboxes, mailboxes, location))
    | `ResentSender mailbox ->
      Value.(B (Field.of_string_exn "Resent-Sender", Value.Mailbox, mailbox, location))
    | `ResentTo addresses ->
      Value.(B (Field.of_string_exn "Resent-To", Value.Addresses, addresses, location))
    | `ResentCc addresses ->
      Value.(B (Field.of_string_exn "Resent-Cc", Value.Addresses, addresses, location))
    | `ResentBcc addresses ->
      Value.(B (Field.of_string_exn "Resent-Bcc", Value.Addresses, addresses, location))
    | `ResentMessageID message_id ->
      Value.(B (Field.of_string_exn "Resent-Message-ID", Value.MessageID, message_id, location))
    | `ResentReplyTo addresses ->
      Value.(B (Field.of_string_exn "Resent-Reply-To", Value.Addresses, addresses, location))
    | `Lines lines ->
      Value.(L (lines, location))

type end_of_header = [ `End ]

let decoder ~field value buffer =
  { q= Q.from buffer
  ; b= buffer
  ; f= field
  ; v= value
  ; cs= Angstrom.Unbuffered.parse parser }

let decode : type field. field decoder -> field decode =
  (* XXX(dinosaure): about [shift_exn], we trust on [angstrom]. *)
  fun decoder -> match decoder.cs with
  | Angstrom.Unbuffered.Partial { committed; continue; } ->
    Q.N.shift_exn decoder.q committed ; `Await
  | Angstrom.Unbuffered.Fail (committed, _, err) ->
    Q.N.shift_exn decoder.q committed ;
    `Malformed err
  | Angstrom.Unbuffered.Done (committed, (#end_of_header, _)) ->
    Q.N.shift_exn decoder.q committed ;
    `End
  | Angstrom.Unbuffered.Done (committed, ((#Rfc5322.field, _) as v)) ->
    Q.N.shift_exn decoder.q committed ;
    match to_binding v with
    | Value.L (lines, _) as cf -> `Other cf
    | Value.B (field, value, x, _) as cf ->
      let () = match Q.N.peek decoder.q with
        | [ x ] -> decoder.cs <- Angstrom.Unbuffered.parse_incomplete_bigstring parser x
        | _ :: _ | [] -> assert false in
      match Value.equal decoder.v value, Field.equal decoder.f field with
      | Some Refl.Refl, true -> `Field x
      | _ -> `Other cf

let blit_from_string src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let src decoder source off len =
  if off < 0 || len < 0 || off + len > String.length source
  then Fmt.invalid_arg "Invalid bounds"
  else match decoder.cs with
    | Angstrom.Unbuffered.Done _ | Angstrom.Unbuffered.Fail _ ->
      Fmt.invalid_arg "Decoder was done or retrieved an error"
    | Angstrom.Unbuffered.Partial { continue; committed; } ->
      Q.N.shift_exn decoder.q committed ;
      Q.compress decoder.q ;
      match Q.N.push decoder.q
              ~blit:blit_from_string
              ~length:String.length
              ~off ~len source with
      | Some [ _ ] ->
        let more = Angstrom.Unbuffered.(if len = 0 then Complete else Incomplete) in
        let[@warning "-8"] [ x ] = Q.N.peek decoder.q in
        (* TODO: we should add it at the pattern-matching only if application of
           them is defined - [push] will be applied __before__ [peek]. By this
           way, we can rely on [ke] assumption. *)
        decoder.cs <- continue ~off:0 ~len:(Bigstringaf.length x) x more ;
        Rresult.R.ok ()
      | Some _ -> assert false (* XXX(dinosaure): see [ke] about that. *)
      | None -> Rresult.R.error_msg "Input is too much bigger"


