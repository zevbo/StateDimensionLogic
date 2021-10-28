open! Core
open Src

(* TODO: decrease duplication *)

module type W_state = sig
  type t

  val current_sds_required : Sd.Packed.t Hash_set.t
  val past_sds_required : Sd.Packed.t Hash_set.t
  val sds_estimating : Sd.Packed.t Hash_set.t
  val est : t -> Robot_state_history.t -> Robot_state.t

  (* TODO: should allow more types of uncertainty, as well as a function that gives covariance *)
  val uncertainty : t -> Robot_state_history.t -> float Sd.t -> Uncertianty.t option
end

module type Wo_state = sig
  type t = unit

  val current_sds_required : Sd.Packed.t Hash_set.t
  val past_sds_required : Sd.Packed.t Hash_set.t
  val sds_estimating : Sd.Packed.t Hash_set.t
  val est : t -> Robot_state_history.t -> Robot_state.t
  val est_stateless : Robot_state_history.t -> Robot_state.t

  (* TODO: should allow more types of uncertainty, as well as a function that gives covariance *)
  val uncertainty : t -> Robot_state_history.t -> float Sd.t -> Uncertianty.t option
  val uncertainty_stateless : Robot_state_history.t -> float Sd.t -> Uncertianty.t option
end

module Applicable : sig
  type t
  type model

  val create : (module W_state with type t = 'a) -> 'a -> t
  val create_model : t list -> model
  val apply : Robot_state_history.t -> model -> Robot_state_history.t
end