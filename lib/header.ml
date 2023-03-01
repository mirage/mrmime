type t = Field.field Location.with_location list

let pp_list ?(sep = fun ppf () -> Format.fprintf ppf ",") pp ppf lst =
  let rec go = function
    | [] -> ()
    | [ x ] -> pp ppf x
    | x :: r ->
        Format.fprintf ppf "%a%a" pp x sep ();
        go r
  in
  go lst

let pp =
  (pp_list ~sep:(fun ppf () -> Format.fprintf ppf "@\n")) (fun ppf x ->
      Field.pp ppf (Location.prj x))

let assoc field_name header =
  let f acc (Field.Field (field_name', _, _) as field) =
    if Field_name.equal field_name field_name' then field :: acc else acc
  in
  List.fold_left f [] (List.map Location.prj header) |> List.rev

let remove_assoc field_name header =
  let f acc x =
    let (Field.Field (field_name', _, _)) = Location.prj x in
    if Field_name.equal field_name field_name' then acc else x :: acc
  in
  List.fold_left f [] header |> List.rev

let exists field_name t =
  List.exists
    (fun (Field.Field (field_name', _, _)) ->
      Field_name.equal field_name field_name')
    (List.map Location.prj t)

let empty = []
let concat a b = a @ b

let add : type a. Field_name.t -> a Field.t * a -> t -> t =
 fun field_name (w, v) t ->
  let field = Field.Field (field_name, w, v) in
  Location.inj ~location:Location.none field :: t

let add_unless_exists : type a. Field_name.t -> a Field.t * a -> t -> t =
 fun field_name (w, v) t ->
  if exists field_name t then t else add field_name (w, v) t

let replace : type a. Field_name.t -> a Field.t * a -> t -> t =
 fun field_name (w, v) t ->
  let rec replace acc = function
    | [] ->
        let field =
          Location.(inj ~location:none (Field.Field (field_name, w, v)))
        in
        List.rev (field :: acc)
    | field :: rest ->
        let (Field.Field (field_name', _, _)) = Location.prj field in
        if Field_name.equal field_name field_name' then
          copy
            (Location.(inj ~location:none (Field.Field (field_name, w, v)))
            :: acc)
            rest
        else replace (field :: acc) rest
  and copy acc rest = List.rev_append rest acc |> List.rev in
  replace [] t

let of_list = List.map (Location.inj ~location:Location.none)
let of_list_with_location x = x
let to_list_with_location x = x
let to_list = List.map Location.prj

let content_type header =
  let content : Content_type.t ref = ref Content_type.default in
  List.iter
    (function
      | Field.Field (field_name, Field.Content, v) ->
          if Field_name.equal field_name Field_name.content_type then
            content := v
      | _ -> ())
    (List.map Location.prj header);
  !content

let content_encoding header =
  let mechanism : Content_encoding.t ref = ref Content_encoding.default in
  List.iter
    (function
      | Field.Field (field_name, Field.Encoding, v) ->
          if Field_name.equal field_name Field_name.content_encoding then
            mechanism := v
      | _ -> ())
    (List.map Location.prj header);
  !mechanism

let message_id header =
  let rec go = function
    | [] -> None
    | Field.Field (field_name, Field.MessageID, (v : MessageID.t)) :: tl ->
        if Field_name.equal field_name Field_name.message_id then Some v
        else go tl
    | _ :: tl -> go tl
  in
  go (List.map Location.prj header)

let length t = List.length t

module Decoder = struct
  open Angstrom

  let is_wsp = function ' ' | '\t' -> true | _ -> false

  let field g =
    Field_name.Decoder.field_name >>= fun field_name ->
    skip_while is_wsp *> char ':' *> Field.Decoder.field ?g field_name

  let with_location p =
    pos >>= fun a ->
    p >>= fun v ->
    pos >>| fun b ->
    let location = Location.make a b in
    Location.inj ~location v

  let header g = many (with_location (field g))
end

module Encoder = struct
  include Prettym

  let noop = ((fun ppf () -> ppf), ())
  let field ppf x = Field.Encoder.field ppf x
  let header ppf x = (list ~sep:noop field) ppf (List.map Location.prj x)
end

let to_stream x = Encoder.to_stream Encoder.header x
