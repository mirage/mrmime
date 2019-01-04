module Make (Functor : sig type 'a t end) = struct
  type t = ..

  module type Extension = sig
    type x
    type t += T of x
  end

  type 'a extension = (module Extension with type x = 'a)
  type instance = V : 'a * 'a Functor.t -> instance

  let handlers = Hashtbl.create 16

  module Injection (X : sig type t val instance : t Functor.t end) = struct
    type x = X.t
    type t += T of x

    let () =
      let instance = X.instance in
      Hashtbl.add handlers
        (Obj.extension_id [%extension_constructor T])
        (function T x -> V (x, instance) | _ -> raise Not_found)
  end

  let inj (type a) (f : a Functor.t) : a extension =
    (module Injection (struct type t = a let instance = f end))

  let prj (t : t) =
    let rec go = function
      | [] -> assert false
      | x :: r -> (try x t with Not_found -> go r) in
    go (Hashtbl.find_all handlers (Obj.extension_id (Obj.extension_constructor t)))
end

module type DECODER = sig
  type decoder
  type error
  type output

  type decode = [ `Await | `End | `Flush of output | `Error of error ]

  val decoder : unit -> decoder
  val src : decoder -> Bytes.t -> int -> int -> unit
  val decode : decoder -> decode

  val string_of_error : error -> string
end

[@@@warning "-32"]

let parser_from_decoder
  (type o) (module Dec : DECODER with type output = o) ~(write:(o -> unit)) end_of_body =
  let open Angstrom in

  let dec = Dec.decoder () in

  let (<.>) f g = fun x ~off ~len -> f (g x ~off ~len) in


  let check_end_of_body =
    let expected_len = String.length end_of_body in
    Unsafe.peek expected_len
      (String.equal end_of_body <.> Bigstringaf.substring) in

  let trailer =
    let rec finish () = match Dec.decode dec with
      | `Await -> assert false
      | `Flush x -> write x ; finish ()
      | `End -> commit
      | `Error err -> fail (Dec.string_of_error err)

    and go () = match Dec.decode dec with
      | `Await ->
        Dec.src dec Bytes.empty 0 0 ; finish ()
      | `Flush x -> write x ; go ()
      | `End -> commit
      | `Error err -> fail (Dec.string_of_error err) in

    go () in

  fix @@ fun m ->
  let choose chunk = function
    | true ->
      let chunk = Bytes.sub chunk 0 (Bytes.length chunk - 1) in
      Dec.src dec chunk 0 (Bytes.length chunk) ; trailer
    | false ->
      Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0] ;
      Dec.src dec chunk 0 (Bytes.length chunk) ;
      advance 1 *> m in

  Unsafe.take_while ((<>) end_of_body.[0]) Bigstringaf.substring
  >>= fun chunk ->

  let rec go () = match Dec.decode dec with
    | `Await ->
      let chunk' = Bytes.create (String.length chunk + 1) in
      Bytes.blit_string chunk 0 chunk' 0 (String.length chunk) ;
      commit *> check_end_of_body >>= choose chunk'
    | `Flush x -> write x ; go ()
    | `End -> commit
    | `Error err -> fail (Dec.string_of_error err) in

  go ()
