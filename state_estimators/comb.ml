open! Core
open Src

type switch_res =
  | Est_1
  | Est_2

module Statics (Est1 : Est.W_state) (Est2 : Est.W_state) = struct
  exception Unequal_estimation

  let sds_estimating =
    let diff = Hash_set.diff Est1.sds_estimating Est2.sds_estimating in
    if Hash_set.length diff > 0
    then raise Unequal_estimation
    else Hash_set.copy Est1.sds_estimating
  ;;

  let current_sds_required =
    Hash_set.union Est1.current_sds_required Est2.current_sds_required
  ;;

  let past_sds_required = Hash_set.union Est1.past_sds_required Est2.past_sds_required
end

module E (Est1 : Est.W_state) (Est2 : Est.W_state) = struct
  include Statics (Est1) (Est2)

  type t =
    { est1 : Est1.t
    ; est2 : Est2.t
    ; switch : Robot_state_history.t -> switch_res
    }

  let create est1 est2 switch = { est1; est2; switch }

  (* Maybe should still update the one we're not using? *)
  let est t state_history =
    match t.switch state_history with
    | Est_1 -> Est1.est t.est1 state_history
    | Est_2 -> Est2.est t.est2 state_history
  ;;

  (* TODO: should allow more types of uncertainty, as well as a function that gives covariance *)
  let uncertainty t state_history sd =
    match t.switch state_history with
    | Est_1 -> Est1.uncertainty t.est1 state_history sd
    | Est_2 -> Est2.uncertainty t.est2 state_history sd
  ;;
end