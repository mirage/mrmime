open Mrmime

exception NotEqual of string

(* Headers comparison *)
let all_field_names =
  Field_name.
    [
      date; from; sender; reply_to; cc; bcc; subject; message_id; in_reply_to;
      references; comments; keywords; received; return_path; content_type;
      content_encoding; mime_version; content_id; content_description;
      resent_date; resent_from; resent_sender; resent_to; resent_cc; resent_bcc;
      resent_message_id; resent_reply_to;
    ]

let rec compare_sorted_list ((only_in_1, only_in_2) as acc) h1 h2 =
  match (h1, h2) with
  | [], [] -> acc
  | xs, [] -> (xs @ only_in_1, only_in_2)
  | [], xs -> (only_in_1, xs @ only_in_2)
  | x :: xs, y :: ys ->
      if x = y then compare_sorted_list acc xs ys
      else if compare x y < 0 then
        compare_sorted_list (x :: only_in_1, only_in_2) xs h2
      else compare_sorted_list (only_in_1, y :: only_in_2) h1 ys

let sort = List.sort_uniq Stdlib.compare

let compare_header (h : Header.t) (h' : Header.t) =
  let go h h' =
    let all1 =
      List.map (fun name -> (name, Header.assoc name h)) all_field_names
    in
    let all2 = List.map (fun name -> Header.assoc name h') all_field_names in
    List.fold_left2
      (fun (h1, h2) (n, l1) l2 ->
        let only_in_1, only_in_2 =
          compare_sorted_list ([], []) (sort l1) (sort l2)
        in
        ((n, only_in_1) :: h1, (n, only_in_2) :: h2))
      ([], []) all1 all2
  in
  go h h'


let compare_leaf b b' =
  if String.length b = String.length b' then b = b' else false

(* Structure comparison between two email. It checks that:

 - the mail structure (leaf, message and multipart) is the same

 - each corresponding parts of the parent mails have the same number
   of email and the same body length *)
let same_structure (h1, m1) (h2, m2) =
  let rec go (h1, m1) (h2, m2) =
    Utils.(count_header h1 = count_header h2)
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
              | _, _ -> false)
            p1 p2
        else false
    | _, _ -> false
  in
  go (h1, m1) (h2, m2)

let equal ((h1, m1) : Header.t * _ Mail.t) ((h2, m2) : Header.t * _ Mail.t) =
  same_structure (h1, m1) (h2, m2)
