open Core
open Sd_logic
open Simple_example

let logic =
  [%map_open.Sd_lang
    let v = sd_past Sds.v 1 (V 0.0) in
    Rs.set Rs.empty Sds.v (Float.max (v -. 0.5) 0.0)]
;;

let sds_estimating = Set.of_list (module Sd.Packed) [ Sd.pack Sds.v ]
let node = Sd_node.est (Sd_est.create logic sds_estimating)
