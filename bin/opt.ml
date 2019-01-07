let to_result ~error = function
  | Some v -> Ok v
  | None -> Error error
