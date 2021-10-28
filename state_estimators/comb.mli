open! Core
open Src

type switch_res =
  | Est_1
  | Est_2

(** [Est1] and [Est2] must have the same [sds_estimating] *)
module E (Est1 : Est.W_state) (Est2 : Est.W_state) : sig
  include Est.W_state

  val create : Est1.t -> Est2.t -> (Robot_state_history.t -> switch_res) -> t
end