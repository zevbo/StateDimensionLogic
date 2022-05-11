open Core
open Sd_logic
open Sd_func

let logic =
  let+ v = sd_past Sds.v 1 (V 0.0) in
  let diff = 0.1 +. Random.float_range (-0.5) 0.5 in
  Rs.set Rs.empty Sds.v (v +. diff)
;;

let est = Sd_est.create logic [ Sd_est.sd Sds.v ]
