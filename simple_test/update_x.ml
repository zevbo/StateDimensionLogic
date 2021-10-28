open! Core
open Src

type t = unit

let create () = ()
let hash_of_list = Hash_set.of_list (module Sd.Packed)
let current_sds_required = hash_of_list [ Sd.pack Sds.v ]
let past_sds_required = hash_of_list [ Sd.pack Sds.x ]
let sds_estimating = hash_of_list [ Sd.pack Sds.x ]

let est_stateless state_history =
  let new_x =
    Rsh.find_past_def state_history ~default:0.0 1 Sds.x
    +. Rsh.find_exn state_history Sds.v
  in
  let state = Robot_state.create () in
  Robot_state.set state Sds.x new_x
;;

let est (_t : t) = est_stateless
let uncertainty_stateless _state_history _sd = None
let uncertainty _t = uncertainty_stateless

(* TODO: should allow more types of uncertainty, as well as a function that gives covariance *)
