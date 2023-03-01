type 'a t =
  | Date : Date.t t
  | Mailboxes : Mailbox.t list t
  | Mailbox : Mailbox.t t
  | Addresses : Address.t list t
  | MessageID : MessageID.t t
  | Unstructured : Unstructured.t t
  | Content : Content_type.t t
  | Encoding : Content_encoding.t t

type witness = Witness : 'a t -> witness
type field = Field : Field_name.t * 'a t * 'a -> field

let make : type a. Field_name.t -> a t -> a -> field =
 fun field_name w v -> Field (field_name, w, v)

let pp_list ?(sep = fun ppf () -> Format.fprintf ppf "") pp ppf lst =
  let rec go = function
    | [] -> ()
    | [ x ] -> Format.fprintf ppf "%a" pp x
    | x :: r ->
        Format.fprintf ppf "%a%a" pp x sep ();
        go r
  in
  go lst

let pp ppf (Field (field_name, w, v)) =
  let of_witness : type a. a t -> Format.formatter -> a -> unit = function
    | Date -> (
        fun ppf v ->
          match Date.to_ptime v with
          | Ok (v, tz_offset_s) -> Ptime.pp_human ~tz_offset_s () ppf v
          | Error _ -> Date.pp ppf v)
    | Mailboxes -> pp_list Mailbox.pp
    | Mailbox -> Mailbox.pp
    | Addresses -> pp_list Address.pp
    | MessageID -> MessageID.pp
    | Unstructured -> Unstructured.pp
    | Content -> Content_type.pp
    | Encoding -> Content_encoding.pp
  in
  Format.fprintf ppf "%a: @[<hov>%a@]" Field_name.pp field_name (of_witness w) v

let of_field_name : Field_name.t -> witness =
 fun field_name ->
  match String.lowercase_ascii (field_name :> string) with
  | "date" -> Witness Date
  | "from" -> Witness Mailboxes
  | "sender" -> Witness Mailbox
  | "reply-to" -> Witness Addresses
  | "to" -> Witness Addresses
  | "cc" -> Witness Addresses
  | "bcc" -> Witness Addresses
  | "subject" -> Witness Unstructured
  | "message-id" -> Witness MessageID
  | "comments" -> Witness Unstructured
  | "content-type" -> Witness Content
  | "content-transfer-encoding" -> Witness Encoding
  | _ -> Witness Unstructured

let parser : type a. a t -> a Angstrom.t = function
  | Date -> Date.Decoder.date_time
  | Mailboxes -> Mailbox.Decoder.mailbox_list
  | Mailbox -> Mailbox.Decoder.mailbox
  | Addresses -> Address.Decoder.address_list
  | MessageID -> MessageID.Decoder.message_id
  | Unstructured ->
      let open Angstrom in
      Unstructured.Decoder.unstructured () >>= fun v ->
      return (v :> Unstructured.t)
  | Content -> Content_type.Decoder.content
  | Encoding -> Content_encoding.Decoder.mechanism

let encoder : type a. a t -> a Prettym.t = function
  | Date -> Date.Encoder.date
  | Mailbox -> Mailbox.Encoder.mailbox
  | Mailboxes -> Mailbox.Encoder.mailboxes
  | Addresses -> Address.Encoder.addresses
  | MessageID -> MessageID.Encoder.message_id
  | Unstructured -> Unstructured.Encoder.unstructured
  | Content -> Content_type.Encoder.content_type
  | Encoding -> Content_encoding.Encoder.mechanism

let ( <.> ) f g x = f (g x)
let msg x = `Msg x

module Decoder = struct
  open Angstrom

  let field ?g field_name =
    let buf = Bytes.create 0x7f in
    (* XXX(dinosaure): fast allocation. *)
    Unstrctrd_parser.unstrctrd buf >>= fun v ->
    let (Witness w) =
      match Option.bind (Field_name.Map.find_opt field_name) g with
      | None -> of_field_name field_name
      | Some w -> w
    in
    let parser = parser w in
    let res =
      let ( >>| ) x f = Result.map f x in
      let ( >>= ) = Result.bind in
      Unstrctrd.without_comments v
      >>| Unstrctrd.fold_fws
      >>| Unstrctrd.to_utf_8_string
      (* XXX(dinosaure): normalized value can have trailing whitespace
       * such as "value (comment)" returns "value ". Given parser can
       * ignore it (and it does not consume all inputs finally). *)
      >>= (Result.map_error msg
          <.> (parse_string ~consume:Consume.Prefix) parser)
      >>| fun v -> Field (field_name, w, v)
    in
    match res with
    | Ok v -> return v
    | Error _ ->
        return (Field (field_name, Unstructured, (v :> Unstructured.t)))
end

module Encoder = struct
  open Prettym

  let field ppf field =
    let (Field (field_name, w, v)) = field in
    match w with
    | Unstructured ->
        let e = encoder w in
        let separator = match v with `WSP _ :: _ -> ":" | _ -> ": " in
        eval ppf
          [ tbox 1;
            !!Field_name.Encoder.field_name;
            !!string;
            !!e;
            close;
            new_line
          ]
          field_name separator v
    | w ->
        let e = encoder w in
        eval ppf
          [ tbox 1;
            !!Field_name.Encoder.field_name;
            string $ ": ";
            !!e;
            close;
            new_line
          ]
          field_name v
end
