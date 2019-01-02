type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

external is_a_sub : ('a, 'b) bigarray -> int -> ('a, 'b) bigarray -> int -> bool = "caml_bigarray_is_a_sub" [@@noalloc]
external bigarray_physically_equal : ('a, 'b) bigarray -> ('a, 'b) bigarray -> bool = "caml_bigarray_physically_equal" [@@noalloc]

module type V = sig
  type t

  val pp : t Fmt.t
  val sentinel : t
  val weight : t -> int
  val merge : t -> t -> t option
  val physically_equal : t -> t -> bool
end

module RBQ (V : V) = struct
  module Queue = Ke.Fke.Weighted

  type t = {a: V.t array; c: int; m: int; q: (int, Bigarray.int_elt) Queue.t}
  and value = V.t

  let make capacity =
    let q, capacity = Queue.create ~capacity Bigarray.Int in
    { a= Array.make capacity V.sentinel
    ; c= 0
    ; m= capacity
    ; q }

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>a = %a;@ \
                         c = %d;@ \
                         m = %d;@ \
                         q = %a;@] }"
      Fmt.(Dump.array V.pp) t.a
      t.c t.m
      (Queue.dump Fmt.int) t.q

  let available t = Queue.available t.q

  let[@inline always] mask x t = x land (t.m - 1)

  let push t v =
    let i = mask t.c t in
    match Queue.push t.q i with
    | Some q ->
      t.a.(i) <- v ;
      Ok { t with c= succ t.c; q; }
    | None -> Error t

  let shift_exn t =
    let i, q = Queue.pop_exn t.q in
    (t.a.(i), { t with q })

  let cons t v =
    let i = mask t.c t in
    match Queue.cons t.q i with
    | Some q ->
      t.a.(i) <- v ;
      Ok { t with c= succ t.c; q; }
    | None -> Error t

  exception Full

  let cons_exn t v =
    match cons t v with
    | Ok t -> t
    | Error _ -> raise Full

  let weight t =
    Queue.fold (fun a i -> a + V.weight t.a.(i)) 0 t.q

  let to_list t =
    let res = ref [] in
    Queue.rev_iter (fun i -> res := t.a.(i) :: !res) t.q ;
    !res
end

type 'a blitter = 'a -> int -> Bigstringaf.t -> int -> int -> unit

module RBA = Ke.Fke.Weighted

module Buffer = struct
  type t =
    | Bigstring of Bigstringaf.t
    | String of string
    | Bytes of bytes

  let weight = function
    | Bigstring x -> Bigstringaf.length x
    | String x -> String.length x
    | Bytes x -> Bytes.length x

  let sub buffer off len = match buffer with
    | Bigstring x -> Bigstring (Bigstringaf.sub x ~off ~len)
    | String x -> String (String.sub x off len)
    | Bytes x -> Bytes (Bytes.sub x off len)
end

module IOVec = struct
  type t = {buffer: Buffer.t; off: int; len: int}

  let weight {len; _} = len

  let pp _ _ = assert false

  let sentinel =
    let deadbeef = "\222\173\190\239" in
    {buffer= Buffer.String deadbeef; off= 0; len= String.length deadbeef}

  let make buffer off len =
    {buffer; off; len}

  let length {len; _} = len

  let lengthv = List.fold_left (fun a x -> length x + a) 0

  let shift {buffer; off; len} n =
    assert (n <= len) ;
    {buffer; off= off + n; len= len - n}

  let split {buffer; off; len} n =
    assert (n <= len) ;
    ( {buffer= Buffer.sub buffer off n; off= 0; len= n}
    , {buffer= Buffer.sub buffer (off + n) (len - n); off= 0; len= len - n})

  let physically_equal a b =
    match a, b with
    | {buffer= Buffer.Bytes a; _}, {buffer= Buffer.Bytes b; _} -> a == b
    | {buffer= Buffer.Bigstring a; _}, {buffer= Buffer.Bigstring b; _} -> bigarray_physically_equal a b
    | _, _ -> false

  let merge a b =
    match a, b with
    | {buffer= Buffer.Bytes a'; _}, {buffer= Buffer.Bytes b'; _} ->
      assert (a' == b') ;
      if a.off + a.len = b.off
      then Some {buffer= Buffer.Bytes a'; off= a.off; len= a.len + b.len}
      else None
    | {buffer= Buffer.Bigstring a'; _}, {buffer= Buffer.Bigstring b'; _} ->
      assert (bigarray_physically_equal a' b') ;
      if a.off + a.len = b.off
      then Some {buffer= Buffer.Bigstring a'; off= a.off; len= a.len + b.len}
      else None
    | _, _ -> None
end

module RBS = RBQ (IOVec)

type encoder =
  { sched : RBS.t
  ; write : (char, Bigarray.int8_unsigned_elt) RBA.t
  ; flush : (int * (int -> encoder -> unit)) Ke.Fke.t
  ; written : int
  ; received : int }

type 'v state =
  | Flush of {continue : int -> 'v state; iovecs : IOVec.t list}
  | Continue of {continue : encoder -> 'v state; encoder: encoder}
  | End of 'v

let create len =
  let write, _ = RBA.create ~capacity:len Bigarray.Char in
  { sched= RBS.make (len * 2)
  ; write
  ; flush = Ke.Fke.empty
  ; written= 0
  ; received= 0 }

let check iovec {write; _} =
  match iovec with
  | {IOVec.buffer= Buffer.Bigstring x; _} ->
    let buf = RBA.unsafe_bigarray write in
    let len = Bigarray.Array1.dim buf in
    is_a_sub x (Bigarray.Array1.dim x) buf len
  | _ -> false

let shift_buffers n t =
  let rec go rest acc t =
    match RBS.shift_exn t.sched with
    | iovec, shifted ->
      let len = IOVec.length iovec in
      if rest > len
      then go (rest - len) (iovec :: acc)
          { t with sched= shifted
                 ; write= if check iovec t then RBA.N.shift_exn t.write len else t.write }
      else if rest > 0
      then
        let last, rest = IOVec.split iovec rest in
        ( List.rev (last :: acc)
        , { t with sched= RBS.cons_exn shifted rest
                 ; write=
                     if check iovec t
                     then RBA.N.shift_exn t.write (IOVec.length last)
                     else t.write })
      else (List.rev acc, t)
    | exception RBS.Queue.Empty -> (List.rev acc, t) in
  go n [] t

let shift_flushes n t =
  let rec go t =
    try
      let (threshold, f), flush = Ke.Fke.pop_exn t.flush in
      if compare (t.written + n - min_int) (threshold - min_int) >= 0
      then let () = f n {t with flush} in go {t with flush}
      else t
    with Ke.Fke.Empty -> t in
  go t

let shift n t =
  let lst, t = shift_buffers n t in
  ( lst
  , let t = shift_flushes (IOVec.lengthv lst) t in { t with written = t.written + n} )

let has t = RBS.weight t.sched

let drain drain t =
  let rec go rest t =
    match RBS.shift_exn t.sched with
    | iovec, shifted ->
        let len = IOVec.length iovec in
        if rest > len then
          go (rest - len)
            { t with
              sched= shifted
            ; write=
                (if check iovec t then RBA.N.shift_exn t.write len else t.write) }
        else
          { t with
            sched= RBS.cons_exn shifted (IOVec.shift iovec rest)
          ; write= (if check iovec t then RBA.N.shift_exn t.write rest else t.write)
          }
    | exception RBS.Queue.Empty -> t
  in
  go drain t |> fun t -> {t with written= t.written + drain}

let flush k t =
  let t = shift_flushes (has t) t in
  let continue n =
    let t = drain n t in
    k {t with written= t.written + n}
  in
  Flush {continue; iovecs= RBS.to_list t.sched}

let continue continue encoder = Continue {continue; encoder}

let rec schedule k ~length ~buffer ?(off = 0) ?len v t =
  let len = match len with Some len -> len | None -> length v - off in
  match RBS.push t.sched (IOVec.make (buffer v) off len) with
  | Ok sched -> continue k {t with sched; received= t.received + len}
  | Error _ ->
      let max = RBS.available t.sched in
      let k t =
        (schedule [@tailcall]) k ~length ~buffer ~off:(off + max)
          ~len:(len - max) v t
      in
      schedule (flush k) ~length ~buffer ~off ~len:max v t

let schedule_string =
  let length = String.length in
  let buffer x = Buffer.String x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bytes =
  let length = Bytes.length in
  let buffer x = Buffer.Bytes x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bigstring =
  let length = Bigarray.Array1.dim in
  let buffer x = Buffer.Bigstring x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_flush f t = {t with flush= Ke.Fke.push t.flush (t.received, f)}

external identity : 'a -> 'a = "%identity"

let schedulev k l t =
  let rec aux t = function
    | [] -> continue k t
    | (length, off, len, buffer) :: r ->
        schedule
          (fun t -> (aux [@tailcall]) t r)
          ~length ?off ?len ~buffer:identity buffer t
  in
  aux t l

let schedulev_bigstring k l t =
  let rec aux t = function
    | [] -> continue k t
    | buffer :: r ->
        schedule_bigstring (fun t -> (aux [@tailcall]) t r) t buffer
  in
  aux t l

let rec write k ~blit ~length ?(off = 0) ?len buffer t =
  let len = match len with Some len -> len | None -> length buffer - off in
  let available = RBA.available t.write in
  (* XXX(dinosaure): we can factorize the first and the second branch. *)
  if available >= len then
    let areas, write = RBA.N.push_exn t.write ~blit ~length ~off ~len buffer in
    schedulev_bigstring k areas {t with write}
  else if available > 0 then
    let k t =
      (write [@tailcall]) k ~blit ~length ~off:(off + available)
        ~len:(len - available) buffer t
    in
    let areas, write =
      RBA.N.push_exn t.write ~blit ~length ~off ~len:available buffer
    in
    schedulev_bigstring (flush k) areas {t with write}
  else
    let k t = (write [@tailcall]) k ~blit ~length ~off ~len buffer t in
    flush k t

let writev k l t =
  let rec aux t = function
    | [] -> continue k t
    | (blit, length, off, len, buffer) :: r ->
        write (fun t -> (aux [@tailcall]) t r) ~blit ~length ?off ?len buffer t
  in
  aux t l

let bigarray_blit_from_string src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let bigarray_blit_from_bytes src src_off dst dst_off len =
  Bigstringaf.blit_from_bytes src ~src_off dst ~dst_off ~len

let bigarray_blit src src_off dst dst_off len =
  Bigarray.Array1.(blit (sub src src_off len) (sub dst dst_off len))

let bigarray_blit_to_bytes src src_off dst dst_off len =
  Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

let write_string =
  let length = String.length in
  let blit = bigarray_blit_from_string in
  fun ?(off = 0) ?len a k t -> write k ~blit ~length ~off ?len a t

let write_bytes =
  let length = Bytes.length in
  let blit = bigarray_blit_from_bytes in
  fun ?(off = 0) ?len a k t -> write k ~blit ~length ~off ?len a t

let write_bigstring =
  let length = Bigarray.Array1.dim in
  let blit = bigarray_blit in
  fun ?(off = 0) ?len a k t -> write k ~blit ~length ~off ?len a t

let write_char =
  let length _ = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0) ;
    assert (len = 1) ;
    Bigstringaf.set dst dst_off src
  in
  fun a k t -> write k ~length ~blit ~off:0 ~len:1 a t

let write_uint8 =
  let length _ = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0) ;
    assert (len = 1) ;
    Bigstringaf.set dst dst_off (Char.unsafe_chr src)
  in
  fun a k t -> write k ~length ~blit ~off:0 ~len:1 a t

module Box = struct
  let o _kind k t = continue k t
  let c k t = continue k t
end

module type EndianBigstringSig = EndianBigstring.EndianBigstringSig
module type EndianBytesSig = EndianBytes.EndianBytesSig

module type SE = sig
  val write_uint16 : int -> (encoder -> 'v state) -> encoder -> 'v state
  val write_uint32 : int32 -> (encoder -> 'v state) -> encoder -> 'v state
  val write_uint64 : int64 -> (encoder -> 'v state) -> encoder -> 'v state
end

module Make (EBigstring : EndianBigstringSig) : SE = struct
  let _length _ = assert false

  let write_uint16 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 2) ;
      EBigstring.set_int16 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:2 a t

  let write_uint32 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 4) ;
      EBigstring.set_int32 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:4 a t

  let write_uint64 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 8) ;
      EBigstring.set_int64 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:8 a t
end

module LE = Make (EndianBigstring.LittleEndian_unsafe)
module BE = Make (EndianBigstring.BigEndian_unsafe)

