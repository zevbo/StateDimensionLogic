open! Core
open Sd_logic
open Sd_func

type t =
  { error : float
  ; real_angle_sd : float Sd.t
  ; angle_sd : float Sd.t
  }

let create error real_angle_sd =
  { error; real_angle_sd; angle_sd = Sd.create "imu_angle" Float.sexp_of_t }
;;

let logic t =
  let+ prev_real_angle_op = sd_past t.real_angle_sd 1 Op
  and+ real_angle = sd t.real_angle_sd
  and+ prev_angle = sd_past t.angle_sd 1 Op in
  let imu_angle =
    match prev_real_angle_op, prev_angle with
    | Some prev_real_angle, Some prev_angle ->
      let diff = prev_real_angle -. real_angle in
      prev_angle +. diff +. (Specs.dt *. Random.float_range (-1. *. t.error) t.error)
    | None, Some prev_angle -> prev_angle
    | None, None | Some _, None -> real_angle
  in
  Rs.set Rs.empty t.angle_sd imu_angle
;;

let est t = Sd_est.create (logic t) [ Sd_est.sd t.angle_sd ]
