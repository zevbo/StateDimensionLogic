open! Core
open! Sd_logic
open! Sd_func

let mpos_sd = Sd.create "mpos" Vec.sexp_of_t
let mangle_sd = Sd.create "mangle" Float.sexp_of_t

let logic =
  let+ prev_pos = sd_past mpos_sd 1 (V Update_pos.starting_pos)
  and+ prev_angle = sd_past mangle_sd 1 (V Update_pos.starting_angle)
  and+ lencoder_prev_op = sd_past Sensors.lencoder.pos_sd 1 Op
  and+ rencoder_prev_op = sd_past Sensors.rencoder.pos_sd 1 Op
  and+ lencoder = sd Sensors.lencoder.pos_sd
  and+ rencoder = sd Sensors.rencoder.pos_sd
  and+ prev_imu_op = sd_past Sensors.imu.angle_sd 1 Op
  and+ imu = sd Sensors.imu.angle_sd in
  let movement_angle, angle_diff =
    match prev_imu_op with
    | None -> imu, 0.
    | Some prev_imu -> (imu +. prev_imu) /. 2., imu -. prev_imu
  in
  let get_diff prev_op curr =
    match prev_op with
    | Some prev -> curr -. prev
    | None -> 0.
  in
  let total_movement =
    (get_diff lencoder_prev_op lencoder +. get_diff rencoder_prev_op rencoder)
    *. Specs.radius
    /. 2.
  in
  let pos = Vec.add prev_pos (Vec.rotate (Vec.create total_movement 0.) movement_angle) in
  let rs = Rs.set Rs.empty mpos_sd pos in
  Rs.set rs mangle_sd (prev_angle +. angle_diff)
;;

let est = Sd_est.create logic [ Sd_est.sd mpos_sd; Sd_est.sd mangle_sd ]