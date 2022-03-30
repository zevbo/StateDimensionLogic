open! Core

(* todo: is RobotState.Id.t better than SD.t? *)
(* todo: is it okay that this is shown? probably *)
type 'a t [@@deriving sexp_of, compare]

module Id : sig
  type t [@@deriving compare, sexp]
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end

val create : string -> ('a -> Sexp.t) -> 'a t
val equal : 'a t -> 'b t -> bool
val hash : 'a t -> int
val compare : 'a t -> 'a t -> int
val sexp_of_t : 'a t -> Sexp.t
val id : 'a t -> Id.t

(* Univ_map.find : Univ_map.t -> *)

val to_type_equal_id : 'a t -> 'a Type_equal.Id.t

module Packed : sig
  (* todo: this shouldn't be in the mli*)
  type 'a sd_t = 'a t
  type t = P : _ sd_t -> t [@@deriving sexp_of, equal, compare]

  val to_string : t -> string
  val create : 'a sd_t -> t
  val hash : t -> int
  val compare : t -> t -> int

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end

(* depedent SD *)
module Dep : sig
  module Id : sig
    type t = Rs.t list Sd.t

    val create : string -> t
  end

  type inst_id = int
  type 'a t
  type 'a inst

  val create : Id.t -> string -> 'a -> Sexp.t
  val inst : 'a t -> int -> 'a inst
  val inst_id : 'a inst -> inst_id
end

val pack : 'a t -> Packed.t

type set = Set.M(Packed).t
