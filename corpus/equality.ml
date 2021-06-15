open Mrmime

exception NotEqual of string

let same_structure m1 m2 =
  let rec go m1 m2 =
    match (m1, m2) with
    | Mail.Leaf _, Mail.Leaf _ -> true
    | Message { body = m1'; _ }, Message { body = m2'; _ } -> go m1' m2'
    | Multipart { body = p1; _ }, Multipart { body = p2; _ } ->
        List.for_all2
          (fun m1 m2 ->
            match (m1, m2) with
            | None, None -> true
            | Some m1, Some m2 -> go m1 m2
            | _, _ -> false)
          p1 p2
    | _, _ -> false
  in
  go m1 m2

let equal (m1 : _ Mail.t) (m2 : _ Mail.t) = same_structure m1 m2
