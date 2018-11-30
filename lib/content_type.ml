exception Invalid_token

module Type = struct
  type t = Rfc2045.ty

  let text = `Text
  let image = `Image
  let audio = `Audio
  let video = `Video
  let application = `Application
  let message = `Message
  let multipart = `Multipart

  let ietf token =
    if Iana.Map.mem (String.lowercase_ascii token) Iana.database then
      Some (`Ietf_token token)
    else None

  let extension token =
    if String.length token < 3 then None
    else
      match (token.[0], token.[1]) with
      | ('x' | 'X'), '-' -> (
        try
          String.iter
            (fun chr -> if not (Rfc2045.is_token chr) then raise Invalid_token)
            (String.sub token 2 (String.length token - 2)) ;
          Some (`X_token token)
        with Invalid_token -> None )
      | _ -> None

  let pp ppf = function
    | `Text -> Fmt.string ppf "text"
    | `Image -> Fmt.string ppf "image"
    | `Audio -> Fmt.string ppf "audio"
    | `Video -> Fmt.string ppf "video"
    | `Application -> Fmt.string ppf "application"
    | `Message -> Fmt.string ppf "message"
    | `Multipart -> Fmt.string ppf "multipart"
    | `Ietf_token token -> Fmt.pf ppf "ietf:%s" token
    | `X_token token -> Fmt.pf ppf "x:%s" token

  let to_string = function
    | `Text -> "text"
    | `Image -> "image"
    | `Audio -> "audio"
    | `Video -> "video"
    | `Application -> "application"
    | `Message -> "message"
    | `Multipart -> "multipart"
    | `Ietf_token token | `X_token token -> token

  let compare a b =
    String.(
      compare (lowercase_ascii (to_string a)) (lowercase_ascii (to_string b)))

  let equal a b = compare a b = 0
end

module Subtype = struct
  type t = Rfc2045.subty

  let ietf _token =
    (* XXX(dinosaure): not sure how to check this value. *) None

  let iana ty token =
    let ty = Type.to_string ty in
    match Iana.Map.find (String.lowercase_ascii ty) Iana.database with
    | database ->
        if Iana.Set.mem (String.lowercase_ascii token) database then
          Some (`Iana_token token)
        else None
    | exception Not_found -> None

  let extension token =
    if String.length token < 3 then None
    else
      match (token.[0], token.[1]) with
      | ('x' | 'X'), '-' -> (
        try
          String.iter
            (fun chr -> if not (Rfc2045.is_token chr) then raise Invalid_token)
            (String.sub token 2 (String.length token - 2)) ;
          Some (`X_token token)
        with Invalid_token -> None )
      | _ -> None

  let pp ppf = function
    | `Ietf_token token -> Fmt.pf ppf "ietf:%s" token
    | `Iana_token token -> Fmt.pf ppf "iana:%s" token
    | `X_token token -> Fmt.pf ppf "x:%s" token

  let compare a b =
    match (a, b)
    with
    | ( (`Ietf_token a | `Iana_token a | `X_token a)
      , (`Ietf_token b | `Iana_token b | `X_token b) )
    -> String.(compare (lowercase_ascii a) (lowercase_ascii b))

  let equal a b = compare a b = 0
end

module Parameters = struct
  module X = Map.Make (String)

  type key = string
  type value = Rfc2045.value
  type t = value X.t

  let key key =
    (* XXX(dinosaure): RFC 2045 says:
       - attribute is ALWAYS case-insensitive
       - attribute := token
    *)
    try
      String.iter
        (fun chr -> if not (Rfc2045.is_token chr) then raise Invalid_token)
        key ;
      Some (String.lowercase_ascii key)
    with Invalid_token -> None

  let value value =
    let to_token x =
      try
        String.iter
          (fun chr -> if not (Rfc2045.is_token chr) then raise Invalid_token)
          x ;
        Some (`Token x)
      with Invalid_token -> None
    in
    let need_to_escape = function
      | '\009' | '\010' | '\013' | '\034' | '\092' -> true
      | _ -> false
    in
    let of_escaped_character = function
      | '\009' -> 't'
      | '\010' -> 'n'
      | '\013' -> 'r'
      | c -> c
    in
    let escape_characters x =
      let len = String.length x in
      let buf = Buffer.create len in
      String.iter
        (fun chr ->
          if need_to_escape chr then (
            Buffer.add_char buf '\\' ;
            Buffer.add_char buf (of_escaped_character chr) )
          else Buffer.add_char buf chr )
        x ;
      Buffer.contents buf
    in
    match to_token value with
    | Some v -> v
    | None -> `String (escape_characters value)

  let empty = X.empty

  let mem key t =
    (* XXX(dinosaure): [key] can only exist by [key] function which apply [String.lowercase_ascii]. *)
    X.mem key t

  let add key value t = X.add key value t
  let singleton key value = X.singleton key value
  let remove key t = X.remove key t

  let find key t =
    match X.find key t with x -> Some x | exception Not_found -> None

  let iter f t = X.iter f t
  let pp_key : key Fmt.t = Fmt.string

  let pp_value ppf = function
    | `Token token -> Fmt.string ppf token
    | `String value -> Fmt.pf ppf "%S" value

  let pp ppf t =
    let pp ppf (key, value) = Fmt.pf ppf "%a:%a" pp_key key pp_value value in
    Fmt.Dump.list pp ppf (X.bindings t)

  let value_unescape x =
    let len = String.length x in
    let res = Buffer.create len in
    let pos = ref 0 in
    while !pos < len do
      if
        x.[!pos] = '\\' && !pos < len - 1
        (* XXX(dinosaure): we can avoid this check when [value] takes care about that. *)
      then (
        Buffer.add_char res (Rfc822.of_escaped_character x.[!pos + 1]) ;
        pos := !pos + 2 )
      else (
        Buffer.add_char res x.[!pos] ;
        incr pos )
    done ;
    Buffer.contents res

  let value_compare a b =
    match (a, b) with
    | `Token a, `Token b -> String.compare a b
    | `String a, `Token b | `Token b, `String a ->
        String.compare (value_unescape a) b
    | `String a, `String b ->
        String.compare (value_unescape a) (value_unescape b)

  let value_equal a b =
    match (a, b) with
    | `Token a, `Token b -> String.equal a b
    | `String a, `Token b | `Token b, `String a ->
        String.equal (value_unescape a) b
    | `String a, `String b ->
        String.equal (value_unescape a) (value_unescape b)

  let compare = X.compare value_compare
  let equal = X.equal value_equal

  let of_list lst =
    List.fold_left (fun a (key, value) -> X.add key value a) X.empty lst

  let to_list t = X.bindings t
end

type t = Rfc2045.content

let make ty subty parameters =
  {Rfc2045.ty; subty; parameters= Parameters.to_list parameters}

let pp ppf {Rfc2045.ty; subty; parameters} =
  Fmt.pf ppf "{ @[<hov>type = %a;@ subtype = %a;@ parameters = %a;@] }" Type.pp
    ty Subtype.pp subty (Fmt.hvbox Parameters.pp)
    (Parameters.of_list parameters)

let equal a b =
  Type.equal a.Rfc2045.ty b.Rfc2045.ty
  && Subtype.equal a.Rfc2045.subty b.Rfc2045.subty
  && Parameters.(
       equal (of_list a.Rfc2045.parameters) (of_list b.Rfc2045.parameters))
