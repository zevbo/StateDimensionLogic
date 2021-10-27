open! Core

(* todo: is RobotState.Id.t better than SD.t? *)
(* todo: is it okay that this is shown? probably *)
type 'a t = 'a Type_equal.Id.t [@@deriving sexp_of]

module Packed : sig
  (* todo: this shouldn't be in the mli*)
  type 'a sd_t = 'a t
  type t = P : _ sd_t -> t [@@deriving sexp_of]

  val create : 'a sd_t -> t
  val equal : t -> t -> bool
end

val create : string -> ('a -> Sexp.t) -> 'a t
val pack : 'a t -> Packed.t
val equal : 'a t -> 'b t -> bool