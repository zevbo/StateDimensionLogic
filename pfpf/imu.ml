open! Core
open Sd_logic
open Sd_lang

type t =
  { error_per : float
  ; angle_sd : float Sd.t
  }

let create error_per = { error_per; angle_sd = Sd.create "imu_angle" Float.sexp_of_t }

let logic t =
  let+ prev_angle_op = sd_past t.angle_sd 1 Op
  and+ angle = sd t.angle_sd in
  let imu_angle =
    match prev_angle_op with
    | None -> angle
    | Some prev_angle ->
      let diff = prev_angle -. angle in
      diff *. (1. +. Random.float_range (-1. *. t.error_per) t.error_per)
  in
  Rs.set Rs.empty t.angle_sd imu_angle
;;

let est t = Sd_est.create (logic t) [ Sd.pack t.angle_sd ]
