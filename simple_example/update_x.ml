open Core
open Sd_logic
open Sd_func

let logic =
  let+ x = sd_past Sds.x 1 (V 0.0)
  and+ v = sd Sds.v in
  Rs.set Rs.empty Sds.x (x +. v)
;;

let est = Sd_est.create logic [ Sd_est.sd Sds.x ]