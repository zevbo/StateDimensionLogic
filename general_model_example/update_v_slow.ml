open Core
open Sd_logic
open Simple_example
open Sd_func

let logic =
  let+ v = sd_past Sds.v 1 (V 0.0) in
  print_endline "SLOWING DOWN";
  Rs.set Rs.empty Sds.v (Float.max (v -. 0.5) 0.0)
;;

let node = Sd_node.est (Sd_est.create logic [ Sd.pack Sds.v ])
