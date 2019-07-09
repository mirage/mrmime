module Ordered = Map.Make(Number)
type field = Rfc5322.field
type t = (Field.field * Location.t) Ordered.t

let reduce
  : (Number.t * ([> field ] as 'a) * Location.t) list -> t -> (t * (Number.t * 'a * Location.t) list)
  = fun fields header ->
    List.fold_left
      (fun (header, rest) (n, field, loc) -> match field with
         | #field as field ->
           Ordered.add n (Field.of_rfc5322_field field, loc) header, rest
         | field ->
           header, (n, field, loc) :: rest)
      (header, []) fields
    |> fun (header, rest) -> (header, List.rev rest)

type value = Value : 'a Field.v * 'a -> value

let field_to_value : Field.field -> value =
  fun (Field (field_name, v)) -> Value (Field.field_value field_name, v)

let get field_name header =
  Ordered.fold (fun i (field, loc) a ->
    if Field_name.equal field_name (Field.field_name field)
    then (i, field_to_value field, loc) :: a
    else a) header []

let cardinal t =
  let folder
    : Number.t -> (Field.field * Location.t) -> (Number.t, [ `Msg of string ]) result -> (Number.t, [ `Msg of string ]) result
    = fun _ (field, _) a ->
    let open Rresult.R in
    match field with
    | Field.Field (Content, v) -> bind a (Number.add_int (Content.length v))
    | Field.Field (Resent, v) -> bind a (Number.add_int (Resent.length v))
    | Field.Field (Trace, v) -> bind a (Number.add_int (Trace.length v))
    | _ -> map Number.succ a in
  let res = Ordered.fold folder t (Ok Number.zero) in
  match res with
  | Ok length -> length
  | Error (`Msg err) -> invalid_arg err (* XXX(dinosaure): should never occur. *)

let add field t =
  Ordered.add (cardinal t) (field, Location.none) t

let add_or_replace (Field.Field (field_name, v) as field) t =
  let exception Exists of Number.t in
  try
    Ordered.iter
      (fun n Field.(Field (field_name', v'), _) ->
         match Field.equal field_name field_name' with
         | Some Refl.Refl -> raise_notrace (Exists n)
         | None -> ())
      t ; add field t
  with Exists n ->
    Ordered.add n (field, Location.none) t

let ( & ) = add

let pp : t Fmt.t = fun ppf t ->
  Fmt.Dump.iter_bindings
    Ordered.iter
    Fmt.(always "header")
    Fmt.nop
    Fmt.(fun ppf (Field.Field (k, v)) ->
        match k with
        | Resent -> Resent.pp ppf v
        | Trace -> Trace.pp ppf v
        | Content -> Content.pp ppf v
        | k ->
          Dump.pair
            (using Field.to_field_name Field_name.pp)
            (Field.pp_of_field_name k) ppf (k, v))
    ppf (Ordered.map fst t)

let pp_value ppf = fun (Value (k, v)) ->
  Field.pp_of_field_value k ppf v

let empty = Ordered.empty

let content header =
  let content : Content.t option ref = ref None in
  Ordered.iter (fun _ -> function
      | Field.Field (Field.Content, v), _ -> content := Some v
      | _ -> ()) header ;
  match !content with
  | Some content -> content
  | None -> Content.default

module Encoder = struct
  include Encoder

  let epsilon = (fun t () -> t), ()
  let field ppf (_, (x, _)) = Field.Encoder.field ppf x
  let header ppf x = (list ~sep:epsilon field) ppf (Ordered.bindings x)
end

let to_string x = Encoder.to_string Encoder.header x
let to_stream x = Encoder.to_stream Encoder.header x
