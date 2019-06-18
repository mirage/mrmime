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
      Ok (`Ietf_token token)
    else Rresult.R.error_msgf "%S is not an IETF token" token

  let extension token =
    if String.length token < 3
    then Rresult.R.error_msgf "Extension token MUST have, at least, 3 bytes: %S" token
    else
      match (token.[0], token.[1]) with
      | ('x' | 'X'), '-' -> (
        try
          String.iter
            (fun chr -> if not (Rfc2045.is_token chr) then raise Invalid_token)
            (String.sub token 2 (String.length token - 2)) ;
          Ok (`X_token token)
        with Invalid_token ->
          Rresult.R.error_msgf "Extension token %S does not respect standards" token)
      | _ -> Rresult.R.error_msgf "An extension token MUST be prefixed by [X-]: %S" token

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
  let default = `Text
end

module Subtype = struct
  type t = Rfc2045.subty

  let ietf token =
    (* XXX(dinosaure): not sure how to check this value. *)
    Ok (`Ietf_token token)

  let iana ty token =
    let ty = Type.to_string ty in
    match Iana.Map.find (String.lowercase_ascii ty) Iana.database with
    | database ->
        if Iana.Set.mem (String.lowercase_ascii token) database then
          Ok (`Iana_token token)
        else Rresult.R.error_msgf "Subtype %S does not exist" token
    | exception Not_found -> Rresult.R.error_msgf "Type %S does not exist" ty

  let iana_exn ty token = match iana ty token with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let v ty token = iana_exn ty token

  let extension token =
    if String.length token < 3
    then Rresult.R.error_msgf "Extension token MUST have, at least, 3 bytes: %S" token
    else
      match (token.[0], token.[1]) with
      | ('x' | 'X'), '-' -> (
        try
          String.iter
            (fun chr -> if not (Rfc2045.is_token chr) then raise Invalid_token)
            (String.sub token 2 (String.length token - 2)) ;
          Ok (`X_token token)
        with Invalid_token ->
          Rresult.R.error_msgf "Extension token %S does not respect standards" token)
      | _ -> Rresult.R.error_msgf "An extension token MUST be prefixed by [X-]: %S" token

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
  let default = `Iana_token "plain"
end

module Parameters = struct
  module Map = Map.Make (String)

  type key = string
  type value = Rfc2045.value
  type t = value Map.t

  let key key =
    (* XXX(dinosaure): RFC 2045 says:
       - attribute is ALWAYS case-insensitive
       - attribute := token
    *)
    try
      String.iter
        (fun chr -> if not (Rfc2045.is_token chr) then raise Invalid_token)
        key ;
      Ok (String.lowercase_ascii key)
    with Invalid_token ->
      Rresult.R.error_msgf "Key %S does not respect standards" key

  let key_exn x = match key x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let k x = key_exn x

  exception Invalid_utf8

  let value v =
    let to_token x =
      try
        String.iter
          (fun chr -> if not (Rfc2045.is_token chr) then raise Invalid_token)
          x ;
        Ok (`Token x)
      with Invalid_token ->
        Rresult.R.error_msgf "Value %S does not respect standards" v
    in
    (* XXX(dinosaure): [is_quoted_pair] accepts characters \000-\127. UTF-8
       extends to \000-\255. However, qtext invalids some of them: \009, \010,
       \013, \032, \034 and \092. Most of them need to be escaped.

       About \032, this case is little bit weird when [qcontent] accepts [FWS].
       At the end, \032, is possible in a quoted-string however, number of it
       does not look significant - so we don't try to escape it. *)
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
    let utf8 x =
      try
        Uutf.String.fold_utf_8
          (fun () _pos -> function `Malformed _ -> raise Invalid_utf8
            | `Uchar _ -> () )
          () x ;
        Ok x
      with Invalid_utf8 ->
        Rresult.R.error_msgf "Value %S is not a valid UTF-8 string" x
    in
    match to_token v with
    | Ok _ as v -> v
    | Error _ ->
        (* UTF-8 respects an interval of values and it's possible to have an
         invalid UTF-8 string. So we need to check it. UTF-8 is a superset of
         ASCII, so we need, firstly to check if it's a valid UTF-8 string. In
         this case, and mostly because we can escape anything (see
         [is_quoted_pair]), we do a pass to escape some of ASCII characters only
         then.

         At the end, if [value] is a valid UTF-8 string, we will don't have a
         problem to encode it if we take care to escape invalid [qtext]
         characters.

         However, order is really important semantically. UTF-8 -> escape
         expects a special process to decoder (escape -> UTF-8). About history,
         unicorn and so on, it should be the best to keep this order. *)
        Rresult.R.(utf8 v >>| escape_characters >>| fun x -> `String x)

  let value_exn x = match value x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err

  let v x = value_exn x

  let empty = Map.empty

  let mem key t =
    (* XXX(dinosaure): [key] can only exist by [key] function which apply [String.lowercase_ascii]. *)
    Map.mem key t

  let add key value t = Map.add key value t
  let singleton key value = Map.singleton key value
  let remove key t = Map.remove key t

  let find key t =
    match Map.find key t with x -> Some x | exception Not_found -> None

  let iter f t = Map.iter f t
  let pp_key : key Fmt.t = Fmt.string

  let pp_value ppf = function
    | `Token token -> Fmt.string ppf token
    | `String value -> Fmt.pf ppf "%S" value

  let pp ppf t =
    let pp ppf (key, value) = Fmt.pf ppf "%a:%a" pp_key key pp_value value in
    Fmt.Dump.list pp ppf (Map.bindings t)

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

  let compare = Map.compare value_compare
  let equal = Map.equal value_equal

  let of_list lst =
    List.fold_left (fun a (key, value) -> Map.add key value a) Map.empty lst

  let to_list t = Map.bindings t

  let default = Map.add "charset" (`Token "us-ascii") Map.empty
end

type t = Rfc2045.content

let default =
  { Rfc2045.ty = Type.default
  ; subty = Subtype.default
  ; parameters = Parameters.(to_list default) }

let ty { Rfc2045.ty; _ } = ty
let subty { Rfc2045.subty; _ } = subty
let parameters { Rfc2045.parameters; _ } = parameters

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

module Encoder = struct
  open Encoder

  let ty ppf = function
    | `Text -> string ppf "text"
    | `Image -> string ppf "image"
    | `Audio -> string ppf "audio"
    | `Video -> string ppf "video"
    | `Application -> string ppf "application"
    | `Message -> string ppf "message"
    | `Multipart -> string ppf "multipart"
    | `Ietf_token v -> string ppf v
    | `X_token v -> using (fun v -> "X-" ^ v) string ppf v

  let subty ppf = function
    | `Ietf_token v -> string ppf v
    | `Iana_token v -> string ppf v
    | `X_token v -> using (fun v -> "X-" ^ v) string ppf v

  let value =
    using
      (function `Token x -> `Atom x | `String x -> `String x)
      Mailbox.Encoder.word

  let parameter ppf (key, v) =
    eval ppf [ box; !!string; cut; char $ '='; cut; !!value; close ]
      key v

  let parameters ppf parameters =
    let sep ppf () = eval ppf [ char $ ';'; fws ] in
    eval ppf [ box; !!(list ~sep:(sep, ()) parameter); close ] parameters

  let content_type ppf t =
    match t.Rfc2045.parameters with
    | [] ->
      eval ppf
        [ bbox; !!ty; cut; char $ '/'; cut; !!subty; close ]
        t.Rfc2045.ty t.Rfc2045.subty
    | _ ->
      eval ppf
        [ bbox; !!ty; cut; char $ '/'; cut; !!subty; cut; char $ ';'; fws; !!parameters; close ]
        t.Rfc2045.ty t.Rfc2045.subty t.Rfc2045.parameters
end
