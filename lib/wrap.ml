module Queue : Ke.Sigs.F = Ke.Fke

type t =
  { state : Encoder.encoder
  ; mutable scan_stack : scan_atom list
  ; mutable forma_stack : format_atom list
  ; margin : int
  ; mutable max_index : int
  ; mutable space_left : int
  ; mutable min_space_left : int
  ; mutable current_indent : int
  ; mutable left_total : int
  ; mutable right_total : int
  ; mutable is_new_line : bool
  ; mutable curr_depth : int
  ; mutable queue : atom Queue.t }
and atom =
  { mutable size : int
  ; token : token
  ; length : int }
and token =
  | Text of string
  | Break of int * int
  | Begin of int * box
  | End
  | New_line
  | If_new_line
and box = H | V | HaV | HoV | Box | Fits
and format_atom = Format of box * int
and scan_atom = Scan of int * atom
