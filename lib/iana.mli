module Map : module type of Map.Make(String)
module Set : module type of Set.Make(String)

val iana   : Set.t Map.t
