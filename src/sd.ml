open! Core

type 'a t = 'a Type_equal.Id.t [@@deriving sexp_of]

let create name sexp_of = Type_equal.Id.create ~name sexp_of
let equal = Type_equal.Id.same

module Packed = struct
  type 'a sd_t = 'a t [@@deriving sexp_of]
  type t = P : _ sd_t -> t [@@deriving sexp_of]

  let create t = P t

  let equal t1 t2 =
    match t1, t2 with
    | P sd_t1, P sd_t2 -> equal sd_t1 sd_t2
  ;;
end

let pack = Packed.create
