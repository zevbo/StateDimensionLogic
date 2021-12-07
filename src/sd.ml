open! Core

type 'a t = 'a Type_equal.Id.t

let create name sexp_of = Type_equal.Id.create ~name sexp_of
let equal = Type_equal.Id.same
let hash = Type_equal.Id.hash
let sexp_of_t t = String.sexp_of_t (Type_equal.Id.name t)
let to_type_equal_id t = t

(* unsafe! if two values hash to the same thing, they will throw an error *)
let compare t1 t2 =
  if equal t1 t2
  then 0
  else (
    match Int.compare (hash t1) (hash t2) with
    | 0 -> failwith "Attempted to compare different ts with the same hash value"
    | n -> n)
;;

module Packed = struct
  type 'a sd_t = 'a t
  type t = P : _ sd_t -> t

  let sexp_of_t t =
    match t with
    | P sd_t -> sexp_of_t sd_t
  ;;

  let create t = P t

  let equal t1 t2 =
    match t1, t2 with
    | P sd_t1, P sd_t2 -> equal sd_t1 sd_t2
  ;;

  let hash t =
    match t with
    | P sd_t -> hash sd_t
  ;;

  let compare t1 t2 =
    match t1, t2 with
    | P sd_t1, P sd_t2 -> compare sd_t1 sd_t2
  ;;
end

let pack = Packed.create
