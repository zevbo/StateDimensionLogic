open! Core
open! Sd_logic
open! Sd_lang

let mpos_sd = Sd.create "mpos" Vec.sexp_of_t
let mangle_sd = Sd.create "angle" Float.sexp_of_t

let logic =
  let+ prev_pos = sd_past mpos_sd 1 (V Update_pos.starting_pos)
  and+ prev_angle = sd_past mangle_sd 1 (V Update_pos.starting_angle)
  and+ lencoder = sd Sensors.lencoder.pos_sd
  and+ rencoder = sd Sensors.rencoder.pos_sd
  and+ prev_imu_op = sd_past Sensors.imu.angle_sd 1 Op
  and+ imu = sd Sensors.imu.angle_sd in
  let movement_angle, angle_diff =
    match prev_imu_op with
    | None -> imu, 0.
    | Some prev_imu -> (imu +. prev_imu) /. 2., prev_imu -. imu
  in
  let total_movement = (lencoder +. rencoder) *. Specs.radius /. 2. in
  let pos = Vec.add prev_pos (Vec.rotate (Vec.create total_movement 0.) movement_angle) in
  let rs = Rs.set Rs.empty mpos_sd pos in
  Rs.set rs mangle_sd (prev_angle +. angle_diff)
;;

let est = Sd_est.create logic [ Sd.pack mpos_sd; Sd.pack mangle_sd ]
