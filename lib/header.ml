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

let add field t =
  let n = Number.of_int_exn (Ordered.cardinal t) in
  Ordered.add n (field, Location.none) t

let ( & ) = add

let pp : t Fmt.t = fun ppf t ->
  Fmt.Dump.iter_bindings
    Ordered.iter
    Fmt.(always "header")
    Fmt.nop
    Fmt.(fun ppf (Field.Field (k, v)) ->
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
