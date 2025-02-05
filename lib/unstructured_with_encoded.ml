type elt = [ Unstructured.elt | `Encoded of string * Emile.raw ]
type t = elt list

let pp_elt ppf : elt -> unit = function
  | #Unstrctrd.elt as elt ->
      Format.pp_print_string ppf
        (Unstrctrd.to_utf_8_string (Unstrctrd.of_list [ elt ] |> Result.get_ok))
  | `Open _ | `Close -> ()
  | `Encoded encoded -> Emile.pp_phrase ppf [ `Encoded encoded ]

let pp ppf u =
  Format.pp_print_string ppf "<unstructured_with_encoded ";
  List.iter (pp_elt ppf) u;
  Format.pp_print_char ppf '>'

module Decoder = struct
  let post_process : Unstrctrd.t -> t =
    let is_equals_sign u = Uchar.to_int u = 0x3d in
    let is_question_mark u = Uchar.to_int u = 0x3f in
    let rec encoded buf part = function
      | `Uchar ch0 :: `Uchar ch1 :: rest
        when part = 2 && is_question_mark ch0 && is_equals_sign ch1 -> (
          Buffer.add_string buf "?=";
          match Encoded_word.of_string (Buffer.contents buf) with
          | Ok ew ->
              let charset =
                Encoded_word.charset_to_string ew.Encoded_word.charset
              in
              let data =
                match ew.Encoded_word.encoding with
                | Base64 -> Emile.Base64 ew.Encoded_word.data
                | Quoted_printable ->
                    Emile.Quoted_printable ew.Encoded_word.data
              in
              Ok (`Encoded (charset, data), rest)
          | Error (`Msg _) -> Error ())
      | `Uchar ch :: rest ->
          let part = if is_question_mark ch then part + 1 else part in
          Uutf.Buffer.add_utf_8 buf ch;
          encoded buf part rest
      | _ -> Error ()
    in
    let rec main (acc : t) = function
      | `Uchar ch0 :: `Uchar ch1 :: rest
        when is_equals_sign ch0 && is_question_mark ch1 -> (
          let buf = Buffer.create 16 in
          Buffer.add_string buf "=?";
          match encoded buf 0 rest with
          | Ok (elt, rest') -> main (elt :: acc) rest'
          | Error () -> main (`Uchar ch1 :: `Uchar ch0 :: acc) rest)
      | elt :: rest -> main (elt :: acc) rest
      | [] -> List.rev acc
    in
    fun input -> main [] (input :> t)

  let unstructured_with_encoded () =
    let open Angstrom in
    Unstructured.Decoder.unstructured () >>| post_process
end

module Craft = struct
  let b = Encoded_word.b
  let q = Encoded_word.q
  let sp len = (Unstructured.Craft.sp len :> elt list)
  let v s = (Unstructured.Craft.v s :> elt list)

  let e ~encoding s =
    let ew = Encoded_word.make_exn ~encoding s in
    let charset = Encoded_word.charset_to_string ew.Encoded_word.charset in
    match ew.Encoded_word.encoding with
    | Base64 -> [ `Encoded (charset, Emile.Base64 ew.Encoded_word.data) ]
    | Quoted_printable ->
        [ `Encoded (charset, Emile.Quoted_printable ew.Encoded_word.data) ]

  let compile : elt list list -> t = List.concat
  let concat : t -> t -> t = List.append
  let ( @ ) = concat
end

module Encoder = struct
  open Prettym

  let element : elt t =
   fun ppf -> function
    | `Encoded (charset, Emile.Quoted_printable data) ->
        Encoded_word.Encoder.encoded_word ppf
          { Encoded_word.charset = `Charset charset;
            encoding = Encoded_word.Quoted_printable;
            data
          }
    | `Encoded (charset, Emile.Base64 data) ->
        Encoded_word.Encoder.encoded_word ppf
          { Encoded_word.charset = `Charset charset;
            encoding = Encoded_word.Base64;
            data
          }
    | #Unstructured.elt as elt -> Unstructured.Encoder.element ppf elt

  let unstructured_with_encoded : elt list t =
   fun ppf lst -> list ~sep:Unstructured.Encoder.noop element ppf lst
end
