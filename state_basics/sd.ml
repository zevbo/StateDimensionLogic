open! Core

type 'a t = 'a Type_equal.Id.t

module Id = struct
  module Id = struct
    type t = Type_equal.Id.Uid.t [@@deriving compare, sexp]
  end

  include Id
  include Comparator.Make (Id)
end

let create name sexp_of = Type_equal.Id.create ~name sexp_of
let equal = Type_equal.Id.same
let hash = Type_equal.Id.hash
let sexp_of_t t = String.sexp_of_t (Type_equal.Id.name t)
let to_type_equal_id t = t
let id t = Type_equal.Id.uid (to_type_equal_id t)

let compare x y =
  let module Tid = Type_equal.Id in
  Tid.Uid.compare (Tid.uid x) (Tid.uid y)
;;

module Packed = struct
  module T = struct
    type 'a sd_t = 'a t
    type t = P : _ sd_t -> t

    let sexp_of_t t =
      match t with
      | P sd_t -> sexp_of_t sd_t
    ;;

    let to_string t = Sexp.to_string (sexp_of_t t)
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

  include T
  include Comparator.Make (T)
end

let pack = Packed.create

type set = Set.M(Packed).t
