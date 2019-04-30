type elt =
  Number.t
  * [ `Unsafe of Field_name.t * Unstructured.t
    | `Lines of (string * Location.t) list ]
  * Location.t

type t = elt list

let make fields = fields
let empty = []
