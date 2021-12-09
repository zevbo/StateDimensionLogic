open! Core
open Src

type t = unit

let create () = ()
let hash_of_list = Hash_set.of_list (module Sd.Packed)
let current_sds_required = hash_of_list []
let past_sds_required = hash_of_list [ Sd.pack Sds.v ]
let sds_estimating = hash_of_list [ Sd.pack Sds.v ]

let est_stateless state_history =
  let state = Robot_state.empty in
  let diff = 0.1 +. Random.float_range (-0.5) 0.5 in
  Robot_state.set
    state
    Sds.v
    (diff +. Rsh.find_past_def state_history ~default:0.0 1 Sds.v)
;;

let est (_t : t) = est_stateless
let uncertainty_stateless _state_history _sd = None
let uncertainty _t = uncertainty_stateless

(* TODO: should allow more types of uncertainty, as well as a function that gives covariance *)
