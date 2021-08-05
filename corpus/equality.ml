open Mrmime
module Log = (val Logs.src_log (Logs.Src.create "equality"))

exception Not_equal of string

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
  | (false, p), (false, p') | (true, p), (true, p') ->
      let res = Parameters.equal p p' in
      if not res then
        Log.err (fun m -> m "Content-type parameters are not equal.");
      res
  | _, _ ->
      Log.err (fun m -> m "Content-type parameters are not equal.");
      false

(** [compare_field_value f f'] compares field values [f] and [f']:
   both must have the same witness and are equal according to the
   proper equality function. Equality between [unstructured] values is
   not checked. *)
let compare_field_value :
    type a b. a Mrmime.Field.t -> a -> b Mrmime.Field.t -> b -> bool =
 fun w v w' v' ->
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
          Log.err (fun m -> m "Content types have different subtypes.");
          false)
      else (
        Log.err (fun m -> m "Content types have different subtypes.");
        false)
  | Field.Encoding, Field.Encoding -> Content_encoding.equal v v'
  | Field.Unstructured, Field.Unstructured -> true
  | _, _ ->
      Log.err (fun m ->
          m "Mismatched field value type: %S <> %S." (Utils.field_to_string w)
            (Utils.field_to_string w'));
      false

let rec compare_sorted_list (h1 : Field.field list) (h2 : Field.field list) =
  match (h1, h2) with
  | [], [] -> true
  | _, [] | [], _ ->
      Log.err (fun m -> m "Headers are different.");
      Log.err (fun m ->
          m "[hdrs0]: @[<hov>%a@]." Mrmime.Header.pp Mrmime.Header.(of_list h1));
      Log.err (fun m ->
          m "[hdrs1]: @[<hov>%a@]." Mrmime.Header.pp Mrmime.Header.(of_list h2));
      false
  | ( (Mrmime.Field.Field (name, w, v) as field) :: xs,
      (Mrmime.Field.Field (name', w', v') as field') :: ys ) ->
      let res = Field_name.equal name name' && compare_field_value w v w' v' in
      if not res then
        Log.err (fun m ->
            m "@[<hov>%a@] is not equal with @[<hov>%a@]." Mrmime.Field.pp field
              Mrmime.Field.pp field');
      res && compare_sorted_list xs ys

(** [compare_header h h'] compares headers with the supposition that
   they are in same order.*)
let compare_header (h : Header.t) (h' : Header.t) =
  let res =
    compare_sorted_list (Mrmime.Header.to_list h) (Mrmime.Header.to_list h')
  in
  if not res then
    Log.err (fun m ->
        m "@[<hov>%a@] <> @[<hov>%a@]" Mrmime.Header.pp h Mrmime.Header.pp h');
  res

let compare_leaf (encoding : Mrmime.Content_encoding.t) b b' =
  let b =
    match encoding with
    (* Quoted printable decoding always adds a \n at the end of the
       line*)
    | `Quoted_printable -> b ^ "\n"
    | _ -> b
  in
  let res = String.length b = String.length b' && b = b' in
  if not res then Log.err (fun m -> m "Contents are not equal %S <> %S." b b');
  res

(** [equal h h'] is an equality between two headers. The comparison checks that:

 - the mail structure (leaf, message and multipart) is the same
 - both content are the same
 - some equalities on headers with [compare_header]
*)
let equal ((h1, m1) : Header.t * _ Mail.t) ((h2, m2) : Header.t * _ Mail.t) =
  let rec go (h1, m1) (h2, m2) =
    Log.debug (fun m ->
        m "Header 0 (%d) = Header 1 (%d)?" (Mrmime.Header.length h1)
          (Mrmime.Header.length h2));
    Mrmime.Header.length h1 = Mrmime.Header.length h2
    && compare_header h1 h2
    &&
    match (m1, m2) with
    | Mail.Leaf b1, Mail.Leaf b2 ->
        compare_leaf (Mrmime.Header.content_encoding h1) b1 b2
    | Message (h1', m1'), Message (h2', m2') -> go (h1', m1') (h2', m2')
    | Multipart p1, Multipart p2 ->
        if List.length p1 = List.length p2 then
          List.for_all2
            (fun (h1', m1') (h2', m2') ->
              match (m1', m2') with
              | None, None -> true
              | Some m1', Some m2' -> go (h1', m1') (h2', m2')
              | None, Some (Mail.Leaf "") | Some (Mail.Leaf ""), None -> true
              | _, _ -> false)
            p1 p2
        else false
    | _, _ -> false
  in
  go (h1, m1) (h2, m2)
