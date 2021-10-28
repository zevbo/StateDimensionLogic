open! Core

(* todo: is RobotState.Id.t better than SD.t? *)
(* todo: is it okay that this is shown? probably *)
type 'a t = 'a Type_equal.Id.t

val create : string -> ('a -> Sexp.t) -> 'a t
val equal : 'a t -> 'b t -> bool
val hash : 'a t -> int
val compare : 'a t -> 'a t -> int
val sexp_of_t : 'a t -> Sexp.t

module Packed : sig
  (* todo: this shouldn't be in the mli*)
  type 'a sd_t = 'a t
  type t = P : _ sd_t -> t [@@deriving sexp_of, equal]

  val create : 'a sd_t -> t
  val hash : t -> int
  val compare : t -> t -> int
end

val pack : 'a t -> Packed.t