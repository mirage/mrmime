open Mrmime

exception NotEqual of string

(** [extract_headers h] extracts all classical headers among a
   predefined list (included date, from, sender, to, reply_to) *)
let extract_headers h =
  let to_ = Field_name.v "to" in
  let field_names =
    Field_name.
      [
        date; from; sender; to_; reply_to; cc; bcc; subject; message_id;
        in_reply_to; references; comments; keywords; received; return_path;
        content_type; content_encoding; mime_version; content_id;
        content_description; resent_date; resent_from; resent_sender; resent_to;
        resent_cc; resent_bcc; resent_message_id; resent_reply_to;
      ]
  in
  List.map (fun name -> (name, Header.assoc name h)) field_names

(** [compare_parameters ct ct'] compares [Content_type.t] parameters
   values of [ct] and [ct']. If a boundary parameter is present in
   both [ct] and [ct'] its value is ignored. *)
let compare_parameters ct ct' =
  let open Content_type in
  let param_without_boundary content_type =
    let p = parameters content_type |> Parameters.of_list in
    match boundary content_type with
    | None -> (Content_type.is_multipart content_type, p)
    | Some _ -> (true, Parameters.(remove (k "boundary") p))
  in
  match (param_without_boundary ct, param_without_boundary ct') with
  | (false, p), (false, p') | (true, p), (true, p') -> Parameters.equal p p'
  | _, _ -> false

(** [comapre_field_value f f'] compares field values [f] and [f']:
   both must have the same witness and are equal according to the
   proper equality function. Equality between [unstructured] values is
   not checked. *)
let compare_field_value ~debug (Field.Field (_n, w, v))
    (Field.Field (_n, w', v')) =
  match (w, w') with
  | Field.Date, Field.Date -> Date.equal v v'
  | Field.Mailbox, Field.Mailbox -> Mailbox.equal v v'
  | Field.Mailboxes, Field.Mailboxes ->
      List.for_all2 (fun f f' -> Mailbox.equal f f') v v'
  | Field.Addresses, Field.Addresses ->
      List.for_all2 (fun add add' -> Address.equal add add') v v'
  | Field.MessageID, Field.MessageID -> MessageID.equal v v'
  | Field.Content, Field.Content ->
      if Content_type.(Type.equal (ty v) (ty v')) then
        if Content_type.(Subtype.equal (subty v) (subty v')) then
          compare_parameters v v'
        else (
          Format.printf "Different content subtype.\n";
          false)
      else (
        Format.printf "Different content type.\n";
        false)
  | Field.Encoding, Field.Encoding -> Content_encoding.equal v v'
  | Field.Unstructured, Field.Unstructured -> true
  | _, _ ->
      if debug then
        Format.printf "Mismatched field value type : %s %s.\n"
          (Utils.field_to_string w) (Utils.field_to_string w');
      false

let compare_field_values ~debug vs vs' =
  List.for_all2 (fun f f' -> compare_field_value ~debug f f') vs vs'

let rec compare_sorted_list ~debug (h1 : (Field_name.t * Field.field list) list)
    (h2 : (Field_name.t * Field.field list) list) =
  match (h1, h2) with
  | [], [] -> true
  | _, [] | [], _ ->
      if debug then Format.printf "Headers have different size.\n";
      false
  | (name, values) :: xs, (name', values') :: ys ->
      if Field_name.equal name name' then
        if compare_field_values ~debug values values' then
          compare_sorted_list ~debug xs ys
        else (
          if debug then Format.printf "Mismatched header field.\n";
          false)
      else (
        if debug then Format.printf "Mismatched header names.\n";
        false)

(** [compare_header h h'] compares headers with the supposition that
   they are in same order.*)
let compare_header (h : Header.t) (h' : Header.t) =
  compare_sorted_list (extract_headers h) (extract_headers h')

let compare_leaf b b' =
  if String.length b = String.length b' then b = b' else false

(** [equal h h'] is an equality between two headers. The comparison checks that: 

 - the mail structure (leaf, message and multipart) is the same 
 - both content are the same
 - some equalities on headers with [compare_header]
*)
let equal ~debug ((h1, m1) : Header.t * _ Mail.t)
    ((h2, m2) : Header.t * _ Mail.t) =
  let rec go (h1, m1) (h2, m2) =
    Utils.(count_header h1 = count_header h2)
    && compare_header ~debug h1 h2
    &&
    match (m1, m2) with
    | Mail.Leaf b1, Mail.Leaf b2 -> compare_leaf b1 b2
    | Message (h1', m1'), Message (h2', m2') -> go (h1', m1') (h2', m2')
    | Multipart p1, Multipart p2 ->
        if List.length p1 = List.length p2 then
          List.for_all2
            (fun (h1', m1') (h2', m2') ->
              match (m1', m2') with
              | None, None -> true
              | Some m1', Some m2' -> go (h1', m1') (h2', m2')
              | None, Some (Mail.Leaf "") | Some (Mail.Leaf ""), None ->
                  true (* to correct in mrmime? *)
              | _, _ ->
                  if debug then Format.printf "Mismatched mails structure.\n";
                  false)
            p1 p2
        else false
    | _, _ -> false
  in
  go (h1, m1) (h2, m2)
