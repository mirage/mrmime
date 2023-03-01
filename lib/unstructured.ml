type box = Box | TBox of int | BBox
type elt = [ Unstrctrd.elt | `Open of box | `Close ]
type t = elt list

let pp ppf t =
  let t =
    List.fold_left
      (fun a -> function #Unstrctrd.elt as x -> x :: a | _ -> a)
      [] t
    |> List.rev
  in
  match Unstrctrd.of_list t with
  | Ok l ->
      let s = Unstrctrd.to_utf_8_string l in
      Format.fprintf ppf "<unstrctrd:%s>" s
  | Error _ -> Format.fprintf ppf "<invalid-unstrctrd>"

module Decoder = struct
  let unstructured () =
    let buf = Bytes.create 0x7f in
    Unstrctrd_parser.unstrctrd buf
end

module Craft = struct
  let sp len : elt list = [ (Unstrctrd.wsp ~len :> elt) ]
  let v s = List.init (String.length s) (fun i -> `Uchar (Uchar.of_char s.[i]))
  let compile : elt list list -> t = List.concat
  let concat : t -> t -> t = fun a b -> a @ b
  let ( @ ) = concat
end

module Encoder = struct
  open Prettym

  type uchar = [ `Uchar of Uchar.t ]
  type ok_or_partial = [ `Ok | `Partial ]

  let element : elt t =
   fun ppf -> function
    | `CR -> string ppf "\r"
    | `LF -> string ppf "\n"
    | `Open Box -> eval ppf [ box ]
    | `Open (TBox n) -> eval ppf [ tbox n ]
    | `Open BBox -> eval ppf [ bbox ]
    | `Close -> eval ppf [ close ]
    | `FWS wsp ->
        let ppf = eval ppf [ cut; new_line ] in
        string ppf (wsp :> string)
    | `OBS_NO_WS_CTL chr -> char ppf (chr :> char)
    | `WSP wsp -> eval ppf [ spaces (String.length (wsp :> string)) ]
    | `d0 -> char ppf '\000'
    | `Invalid_char _ -> string ppf "\xEF\xBF\xBD"
    | #uchar as uchar ->
        let output = Stdlib.Buffer.create 4 in
        let encoder = Uutf.encoder `UTF_8 (`Buffer output) in
        (* XXX(dinosaure): [Uutf.encoder_dst <> `Manual]. It's safe. *)
        let[@warning "-8"] (`Ok : ok_or_partial) = Uutf.encode encoder uchar in
        let[@warning "-8"] (`Ok : ok_or_partial) = Uutf.encode encoder `End in
        string ppf (Stdlib.Buffer.contents output)

  let noop = ((fun ppf () -> ppf), ())
  let unstructured : elt list t = fun ppf lst -> list ~sep:noop element ppf lst
end

let of_string x =
  match Unstrctrd.of_string x with
  | Ok (_consumed, v) -> Ok v
  | Error (`Msg err) -> Error (`Msg err)

let to_string x = Prettym.to_string Encoder.unstructured x
