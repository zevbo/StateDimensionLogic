open! Core

type 'a t = 'a Type_equal.Id.t [@@deriving sexp_of]

module Packed = struct
  type 'a sd_t = 'a t
  type t = P : _ sd_t -> t [@@deriving sexp_of]

  let create t = P t
end

let create name sexp_of = Type_equal.Id.create ~name sexp_of
let pack = Packed.create