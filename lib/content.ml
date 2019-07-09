type mail = [ Rfc2045.field | Rfc2045.field_version ]
type part = Rfc2045.field

module Ordered = Map.Make(Number)

type t = (Content_field.field * Location.t) Ordered.t

let find ~default predicate t =
  let exception Found in
  let res : 'a option ref = ref None in
  try
    Ordered.iter (fun _ v -> match predicate v with
        | Some r -> res := Some r ; raise Found
        | None -> ()) t ; default
  with Found -> match !res with
    | Some v -> v
    | None -> default

let equal a b =
  let exception Diff in

  try
    Ordered.iter
      (fun n (Content_field.(Field (field_name, v), _)) ->
         match Ordered.find_opt n b with
         | Some (Content_field.(Field (field_name', v')), _) ->
           ( match Content_field.equal field_name field_name' with
             | Some Refl.Refl ->
               let eq = Content_field.equal_of_field_name field_name in
               if eq v v' then () else raise_notrace Diff
             | None -> raise_notrace Diff )
         | None -> raise_notrace Diff) a ; true
  with Diff -> false

let default : t =
  let content_type = Content_field.(make Type Content_type.default) in
  Ordered.singleton Number.zero (content_type, Location.none)

let make ?encoding ?id content_type =
  let encoding = Option.(encoding >>| Content_field.(make Encoding)) in
  let id = Option.(id >>| Content_field.(make ID)) in

  let indice = ref Number.zero in
  let add_opt v m = match v with
    | Some v ->
      indice := Number.succ !indice ;
      Ordered.add !indice (v, Location.none) m
    | None -> m in

  Ordered.singleton Number.zero (Content_field.(make Type content_type), Location.none)
  |> add_opt encoding
  |> add_opt id

let number t =
  let open Option in
  Ordered.choose_opt t >>| fst

let length t = Ordered.cardinal t

let empty = Ordered.empty

let add field t =
  let number = Number.of_int_exn (Ordered.cardinal t) in
  Ordered.add number (field, Location.none) t

let add_or_replace (Content_field.Field (field_name, v) as field) t =
  let exception Exists of Number.t in
  try
    Ordered.iter
      (fun n Content_field.(Field (field_name', v'), _) ->
         match Content_field.equal field_name field_name' with
         | Some Refl.Refl -> raise_notrace (Exists n)
         | None -> ())
      t ; add field t
  with Exists n ->
    Ordered.add n (field, Location.none) t

let merge merge a b =
  let a = Ordered.bindings a |> List.map snd |> List.map fst in
  let b = ref (Ordered.bindings b |> List.map snd |> List.map fst) in

  let remove x l =
    let once = ref false in
    let res = ref [] in
    List.iter (fun y ->
        if Content_field.field_equal x y && not !once
        then ( once := true )
        else ( res := y :: !res )) l ;
    List.rev !res in

  let r =
    List.fold_left
      (fun r (Content_field.Field (f, v) as x) ->
         match List.find_opt (fun (Content_field.Field (f', _)) -> Option.is_some (Content_field.equal f f')) !b with
         | Some y -> b := remove y !b ; merge (Some x) (Some y) :: r
         | None -> merge (Some x) None :: r)
      [] a in
  let r = List.rev_append (List.map (fun y -> merge None (Some y)) !b) r in
  let r = List.partition Option.is_some (List.rev r) |> fun (r, _) -> List.map Option.get_exn r in
  let r = List.mapi (fun i x -> Number.of_int_exn i, (x, Location.none)) r in
  Ordered.of_seq (List.to_seq r)


let ( & ) = add

let content_type : t -> Content_type.t = fun t ->
  find ~default:Content_type.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Type -> Some v
       | _ -> None) t

let add_parameter ~key ~value t =
  let content_type = content_type t in
  let content_type = Content_type.with_parameter content_type (key, value) in
  add_or_replace Content_field.(Field (Type, content_type)) t

let ty : t -> Content_type.Type.t = fun t ->
  find ~default:Content_type.Type.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Type -> Some v.ty
       | _ -> None) t

let subty : t -> Content_type.Subtype.t = fun t ->
  find ~default:Content_type.Subtype.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Type -> Some v.subty
       | _ -> None) t

let encoding : t -> Content_encoding.t = fun t ->
  find ~default:Content_encoding.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Encoding -> Some v
       | _ -> None) t

let parameters : t -> Content_type.Parameters.t = fun t ->
  find ~default:Content_type.Parameters.default
    (fun (Content_field.Field (field_name, v), _) -> match field_name with
       | Content_field.Type ->
         Some (Content_type.Parameters.of_list v.parameters)
       | _ -> None) t

let boundary : t -> Rfc2045.value option = fun t ->
  List.assoc_opt "boundary" (Content_type.Parameters.to_list (parameters t))

let ( <.> ) f g = fun x -> f (g x)
let is_discrete : t -> bool = Content_type.Type.is_discrete <.> ty
let is_multipart : t -> bool = Content_type.Type.is_multipart <.> ty
let is_message : t -> bool = Content_type.Type.is_message <.> ty

let pp : t Fmt.t = fun ppf t ->
  Fmt.Dump.iter_bindings
    Ordered.iter
    Fmt.(always "content")
    Fmt.nop
    Fmt.(fun ppf (Content_field.Field (k, v)) ->
        Dump.pair
          (using Content_field.to_field_name Field_name.pp)
          (Content_field.pp_of_field_name k) ppf (k, v))
    ppf (Ordered.map fst t)

let reduce_as_part
  : ((Number.t * ([> part] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list)
  = fun fields content ->
    List.fold_left
      (fun (content, rest) (n, field, loc) -> match field with
         | #part as field ->
           Ordered.add n (Content_field.of_rfc2045_field field, loc) content, rest
         | field ->
           content, (n, field, loc) :: rest)
      (content, []) fields
  |> fun (content, rest) -> (content, List.rev rest)

let reduce_as_mail
  : ((Number.t * ([> mail] as 'a) * Location.t) list) -> t -> (t * (Number.t * 'a * Location.t) list)
  = fun fields content ->
    List.fold_left
      (fun (content, rest) (n, field, loc) -> match field with
         | #part as field ->
           Ordered.add n (Content_field.of_rfc2045_field field, loc) content, rest
         | #mail -> content, rest
         (* TODO: we discard ["MIME-Version"]. *)
         | field ->
           content, (n, field, loc) :: rest)
      (content, []) fields
  |> fun (content, rest) -> (content, List.rev rest)

module Encoder = struct
  open Encoder

  let field_name = Field_name.Encoder.field_name
  let content_type = Content_type.Encoder.content_type
  let content_encoding = Content_encoding.Encoder.mechanism
  let message_id = MessageID.Encoder.message_id
  let unstructured = Unstructured.Encoder.unstructured
  let mime_version = Mime_version.Encoder.mime_version

  let field_and_value field_value value_encoding ppf value =
    eval ppf [ !!field_name; char $ ':'; spaces 1
             ; bbox; !!value_encoding; close; new_line ] field_value value

  let content_type = field_and_value Field_name.content_type content_type
  let content_encoding = field_and_value Field_name.content_encoding content_encoding
  let content_id = field_and_value Field_name.content_id message_id
  let content_description = field_and_value Field_name.content_description unstructured
  let mime_version = field_and_value Field_name.mime_version mime_version
  let content field = field_and_value field unstructured
  let content_unsafe field = field_and_value field unstructured
  let content_field field = field_and_value field unstructured

  let content_as_part ppf (_, (Content_field.Field (field_name, v), _)) = match field_name with
    | Content_field.Type -> content_type ppf v
    | Content_field.Encoding -> content_encoding ppf v
    | Content_field.ID -> content_id ppf v
    | Content_field.Description -> content_description ppf v
    | Content_field.Field field_name -> content_field field_name ppf v

  let epsilon = (fun t () -> t), ()
  let content ppf t = (list ~sep:epsilon content_as_part) ppf (Ordered.bindings t)
end
