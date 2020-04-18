[@@@warning "-32"]

module type V = sig
  type t

  val pp : t Fmt.t
  val sentinel : t
  val weight : t -> int
  val merge : t -> t -> t option
end

module RBQ (V : V) = struct
  module Queue = Ke.Fke.Weighted

  type t = {a: V.t array; c: int; m: int; q: (int, Bigarray_compat.int_elt) Queue.t}
  (* XXX(dinosaure): [ke] is limited to [Bigarray_compat.kind]. We make an [array]
     which will contain values and [q] will contain index of them. Length of [a]
     is length of [q]. By this way, length is a power of two and [a] follows
     same assertions (see [mask]) as [Ke].

     [c] will be the cursor in [a]. [m] is the capacity. It's a good example of
     [ke] with something else than [Bigarray_compat.kind]. *)

  let make capacity =
    let q, capacity = Queue.create ~capacity Bigarray_compat.Int in
    { a= Array.make capacity V.sentinel
    ; c= 0
    ; m= capacity
    ; q }

  let pp ppf t =
    let a = Array.make (Queue.length t.q) V.sentinel in
    let x = ref 0 in
    Queue.iter (fun i -> a.(!x) <- t.a.(i) ; incr x) t.q ;
    Fmt.pf ppf "{ @[<hov>a = %a;@ \
                         c = %d;@ \
                         m = %d;@ \
                         q = %a;@] }"
      Fmt.(Dump.array V.pp) a
      t.c t.m
      (Queue.dump Fmt.int) t.q

  let available t = Queue.available t.q
  let is_empty t = Queue.is_empty t.q

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

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;

    Fmt.pf ppf "@,"
  done

module RBA = Ke.Fke.Weighted

module Buffer = struct
  type t =
    | Bigstring of Bigstringaf.t
    | String of string
    | Bytes of bytes

  let pp ppf = function
    | Bigstring x -> pp_scalar ~length:Bigstringaf.length ~get:Bigstringaf.get ppf x
    | String x -> pp_scalar ~length:String.length ~get:String.get ppf x
    | Bytes x -> pp_scalar ~length:Bytes.length ~get:Bytes.get ppf x

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

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>buffer= @[<hov>%a@];@ \
                         off= %d;@ len= %d;@] }"
      Buffer.pp t.buffer t.off t.len

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

  let merge a b =
    match a, b with
    | {buffer= Buffer.Bytes a'; _}, {buffer= Buffer.Bytes b'; _} ->
      assert (a' == b') ;
      if a.off + a.len = b.off
      then Some {buffer= Buffer.Bytes a'; off= a.off; len= a.len + b.len}
      else None
    | {buffer= Buffer.Bigstring a'; _}, {buffer= Buffer.Bigstring _; _} ->
      if a.off + a.len = b.off
      then Some {buffer= Buffer.Bigstring a'; off= a.off; len= a.len + b.len}
      else None
    | _, _ -> None
end

module RBS = RBQ (IOVec)

type emitter = IOVec.t list -> int

type encoder =
  { sched : RBS.t
  ; write : (char, Bigarray_compat.int8_unsigned_elt) RBA.t
  ; flush : (int * (int -> encoder -> unit)) Ke.Fke.t
  ; written : int
  ; received : int
  ; emitter : emitter }

let pp_flush ppf _ = Fmt.string ppf "#flush"

let pp ppf t =
  Fmt.pf ppf "{ @[<hov>sched= @[<hov>%a@];@ \
                       write= @[<hov>%a@];@ \
                       flush= @[<hov>%a@];@ \
                       written= %d;@ \
                       received= %d;@ \
                       emitter= #emitter;@] }"
    RBS.pp t.sched
    (RBA.pp pp_chr) t.write
    (Ke.Fke.pp pp_flush) t.flush
    t.written t.received

let is_empty t = RBS.is_empty t.sched

(* XXX(dinosaure): [sched] is a queue of [IOVec]. [write] is a
   ring-buffer/[Bigstringaf.t]. [flush] is a queue which can contain
   user-defined operation at a break point. [written] is how many bytes we
   sended to the user (afterwards a *flush* operation). [received] is how many
   bytes we received from the user.

   The goal is to have two ways to fill output:
   - an heavy way with [write_*] operations which will do internally a [blit].
   - a soft way with [shedule_*] operations which will store a pointer.

   The complexity is under [sched] where it stores pointer from user but pointer
   from [write] queue too. Indeed, [write_] operations did not do only a [blit]
   but then they store resulted/*blitted* [Bigstringaf.t] part to [sched].

   When we want to shift a part of [encoder], **all** buffers are stored in
   [sched]. So we need to shift [sched]. However, resulted [IOVec] can be
   physically a part of [write]. In this context, we need to shift [write]. *)

let create ~emitter len =
  let write, _ = RBA.create ~capacity:len Bigarray_compat.Char in
  { sched= RBS.make (len * 2)
  ; write
  ; flush = Ke.Fke.empty
  ; written= 0
  ; received= 0
  ; emitter }

let check iovec {write; _} =
  match iovec with
  | {IOVec.buffer= Buffer.Bigstring x; _} ->
    let buf = RBA.unsafe_bigarray write in
    ( match Overlap.array1 x buf with
    | Some (_, _, _) -> true
    | None -> false )
  | _ -> false

let shift_buffers written t =
  let rec go written acc t =
    match RBS.shift_exn t.sched with
    | iovec, shifted ->
      let len = IOVec.length iovec in
      if written > len
      then go (written - len) (iovec :: acc)
          { t with sched= shifted
                 ; write=
                     if check iovec t
                     then RBA.N.shift_exn t.write len
                     else t.write }
      else if written > 0
      then
        let last, rest = IOVec.split iovec written in
        ( List.rev (last :: acc)
        , { t with sched= RBS.cons_exn shifted rest
                 ; write=
                     if check iovec t
                     then RBA.N.shift_exn t.write (IOVec.length last)
                     else t.write })
      else (List.rev acc, t)
    | exception RBS.Queue.Empty -> (List.rev acc, t) in
  go written [] t

let shift_flushes written t =
  let rec go t =
    try
      let (threshold, f), flush = Ke.Fke.pop_exn t.flush in
      if compare (t.written + written - min_int) (threshold - min_int) >= 0
      then let () = f written {t with flush} in go {t with flush}
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
        if rest >= len then
          go (rest - len)
            { t with
              sched= shifted
            ; write=
                if check iovec t
                then RBA.N.shift_exn t.write len
                else t.write }
        else
          { t with
            sched= RBS.cons_exn shifted (IOVec.shift iovec rest)
          ; write=
              if check iovec t
              then RBA.N.shift_exn t.write rest
              else t.write }
    | exception RBS.Queue.Empty -> t in
  let t = go drain t in { t with written= t.written + drain }

let flush k t =
  let t = shift_flushes (has t) t in
  let n = t.emitter (RBS.to_list t.sched) in
  let t = drain n t in
  k { t with written= t.written + n }

let rec schedule k ~length ~buffer ?(off = 0) ?len v t =
  let len = match len with Some len -> len | None -> length v - off in
  match RBS.push t.sched (IOVec.make (buffer v) off len) with
  | Ok sched ->
    (* TODO: merge [Bigstringaf.t]. *)
    k { t with sched; received= t.received + len }
  | Error _ ->
      let max = RBS.available t.sched in
      let k t =
        (schedule [@tailcall]) k ~length ~buffer ~off:(off + max)
          ~len:(len - max) v t in
      schedule (flush k) ~length ~buffer ~off ~len:max v t

external identity : 'a -> 'a = "%identity"

let kschedule_string =
  let length = String.length in
  let buffer x = Buffer.String x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_string = kschedule_string identity

let kschedule_bytes =
  let length = Bytes.length in
  let buffer x = Buffer.Bytes x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bytes = kschedule_bytes identity

let kschedule_bigstring =
  let length = Bigarray_compat.Array1.dim in
  let buffer x = Buffer.Bigstring x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bigstring = kschedule_bigstring identity

let schedule_flush f t = {t with flush= Ke.Fke.push t.flush (t.received, f)}

let kschedulev k l t =
  let rec go t = function
    | [] -> k t
    | (length, off, len, buffer) :: r ->
      schedule
        (fun t -> (go [@tailcall]) t r)
        ~length ?off ?len ~buffer:identity buffer t
  in go t l

let schedulev = kschedulev identity

let kschedulev_bigstring k l t =
  let rec go t = function
    | [] -> k t
    | buffer :: r ->
      kschedule_bigstring (fun t -> (go [@tailcall]) t r) t buffer
  in go t l

let schedulev_bigstring = kschedulev_bigstring identity

let rec kwrite k ~blit ~length ?(off = 0) ?len buffer t =
  let len = match len with Some len -> len | None -> length buffer - off in
  let available = RBA.available t.write in
  (* XXX(dinosaure): we can factorize the first and the second branch. *)
  if available >= len then
    let areas, write = RBA.N.push_exn t.write ~blit ~length ~off ~len buffer in
    kschedulev_bigstring k areas {t with write}
  else if available > 0 then
    let k t =
      (kwrite [@tailcall]) k ~blit ~length ~off:(off + available)
        ~len:(len - available) buffer t in
    let areas, write = RBA.N.push_exn t.write ~blit ~length ~off ~len:available buffer in
    kschedulev_bigstring (flush k) areas {t with write}
  else
    let k t = (kwrite [@tailcall]) k ~blit ~length ~off ~len buffer t in
    flush k t

let write = kwrite identity

let kwritev k l t =
  let rec go t = function
    | [] -> k t
    | (blit, length, off, len, buffer) :: r ->
        kwrite (fun t -> (go [@tailcall]) t r) ~blit ~length ?off ?len buffer t
  in go t l

let bigarray_blit_from_string src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let bigarray_blit_from_bytes src src_off dst dst_off len =
  Bigstringaf.blit_from_bytes src ~src_off dst ~dst_off ~len

let bigarray_blit src src_off dst dst_off len =
  Bigarray_compat.Array1.(blit (sub src src_off len) (sub dst dst_off len))

let bigarray_blit_to_bytes src src_off dst dst_off len =
  Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

let kwrite_string =
  let length = String.length in
  let blit = bigarray_blit_from_string in
  fun k ?(off = 0) ?len a t -> kwrite k ~blit ~length ~off ?len a t

let write_string = kwrite_string identity

let kwrite_bytes =
  let length = Bytes.length in
  let blit = bigarray_blit_from_bytes in
  fun k ?(off = 0) ?len a t -> kwrite k ~blit ~length ~off ?len a t

let write_bytes = kwrite_bytes identity

let kwrite_bigstring =
  let length = Bigarray_compat.Array1.dim in
  let blit = bigarray_blit in
  fun k ?(off = 0) ?len a t -> kwrite k ~blit ~length ~off ?len a t

let write_bigstring = kwrite_bigstring identity

let kwrite_char =
  let length _ = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0) ;
    assert (len = 1) ;
    Bigstringaf.set dst dst_off src
  in
  fun k a t -> kwrite k ~length ~blit ~off:0 ~len:1 a t

let write_char = kwrite_char identity

let kwrite_uint8 =
  let length _ = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0) ;
    assert (len = 1) ;
    Bigstringaf.set dst dst_off (Char.unsafe_chr src)
  in
  fun k a t -> kwrite k ~length ~blit ~off:0 ~len:1 a t

let write_uint8 = kwrite_uint8 identity

module type S = sig
  val kwrite_uint16 : (encoder -> 'v) -> int -> encoder -> 'v
  val write_uint16 : int -> encoder -> encoder
  val kwrite_uint32 : (encoder -> 'v) -> int32 -> encoder -> 'v
  val write_uint32 : int32 -> encoder -> encoder
  val kwrite_uint64 : (encoder -> 'v) -> int64 -> encoder -> 'v
  val write_uint64 : int64 -> encoder -> encoder
end

module type ENDIAN = sig
  type t = Bigstringaf.t

  val set_int16 : t -> int -> int -> unit
  val set_int32 : t -> int -> int32 -> unit
  val set_int64 : t -> int -> int64 -> unit
end

module Make (X : ENDIAN) : S = struct
  let _length _ = assert false

  let kwrite_uint16 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 2) ;
      X.set_int16 dst dst_off src
    in
    fun k a t -> kwrite k ~length ~blit ~off:0 ~len:2 a t

  let write_uint16 = kwrite_uint16 identity

  let kwrite_uint32 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 4) ;
      X.set_int32 dst dst_off src
    in
    fun k a t -> kwrite k ~length ~blit ~off:0 ~len:4 a t

  let write_uint32 = kwrite_uint32 identity

  let kwrite_uint64 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 8) ;
      X.set_int64 dst dst_off src
    in
    fun k a t -> kwrite k ~length ~blit ~off:0 ~len:8 a t

  let write_uint64 = kwrite_uint64 identity
end

module LE' = struct
  type t = Bigstringaf.t

  let set_int16 = Bigstringaf.set_int16_le
  let set_int32 = Bigstringaf.set_int32_le
  let set_int64 = Bigstringaf.set_int64_le
end

module BE' = struct
  type t = Bigstringaf.t

  let set_int16 = Bigstringaf.set_int16_be
  let set_int32 = Bigstringaf.set_int32_be
  let set_int64 = Bigstringaf.set_int64_be
end

module LE = Make(LE')
module BE = Make(BE')
