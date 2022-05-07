open Core
open Sd_logic
open Sd_func

let logic =
  let+ x = sd Sds.x in
  Rs.set Rs.empty Sds.light_on Float.(x > 50.0)
;;

let est = Sd_est.create logic [ Sd.pack Sds.light_on ]
