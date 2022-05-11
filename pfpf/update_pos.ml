open! Core
open! Sd_logic
open! Sd_func
open Specs

let starting_pos = Vec.create 0. 0.
let starting_angle = 0.

let update_omega_est =
  let logic =
    let+ lomega = sd_past State_sds.lomega 1 (V 0.0)
    and+ romega = sd_past State_sds.romega 1 (V 0.0)
    and+ linput = sd_past State_sds.linput 1 (V 0.0)
    and+ rinput = sd_past State_sds.rinput 1 (V 0.0) in
    let drag_f = drag *. 0.5 *. (lomega +. romega) in
    let update_omega omega input = omega +. ((input -. drag_f) *. dt) in
    let rs = Rs.empty in
    let rs = Rs.set rs State_sds.lomega (update_omega lomega linput) in
    let rs = Rs.set rs State_sds.romega (update_omega romega rinput) in
    rs
  in
  Sd_est.create logic [ Sd_est.sd State_sds.lomega; Sd_est.sd State_sds.romega ]
;;

let update_pos_logic =
  let+ prev_pos = sd_past State_sds.pos 1 (V starting_pos)
  and+ angle = sd_past State_sds.angle 1 (V starting_angle)
  and+ lomega = sd State_sds.lomega
  and+ romega = sd State_sds.romega
  and+ lpos = sd_past State_sds.lpos 1 (V 0.0)
  and+ rpos = sd_past State_sds.rpos 1 (V 0.0) in
  let ldist = lomega *. dt *. radius in
  let rdist = romega *. dt *. radius in
  let del_theta = (rdist -. ldist) /. width in
  let frame_del_pos =
    if Float.(del_theta = 0.0)
    then Vec.create ((rdist +. ldist) /. 2.) 0.0
    else (
      let r = width /. 2. *. (rdist +. ldist) /. (rdist -. ldist) in
      Vec.create (r *. Float.sin del_theta) (r *. (1. -. Float.cos del_theta)))
  in
  let del_pos = Vec.rotate frame_del_pos angle in
  let rs = Rs.set Rs.empty State_sds.angle (angle +. del_theta) in
  let rs = Rs.set rs State_sds.lpos (lpos +. (ldist /. radius)) in
  let rs = Rs.set rs State_sds.rpos (rpos +. (rdist /. radius)) in
  Rs.set rs State_sds.pos (Vec.add prev_pos del_pos)
;;

let update_pos_est =
  Sd_est.create
    update_pos_logic
    [ Sd.pack State_sds.angle
    ; Sd.pack State_sds.pos
    ; Sd.pack State_sds.lpos
    ; Sd.pack State_sds.rpos
    ]
;;
