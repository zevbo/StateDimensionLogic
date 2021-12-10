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

  val create : 'a sd_t -> t
  val hash : t -> int
  val compare : t -> t -> int

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end

val pack : 'a t -> Packed.t

type set = (Packed.t, Packed.comparator_witness) Set.t
