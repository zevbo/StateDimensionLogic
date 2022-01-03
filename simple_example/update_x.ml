open Core
open State_basics
open State_estimators

let logic =
  [%map_open.Sd_lang
    let x = sd_past Sds.x 1 (V 0.0)
    and v = sd Sds.v in
    Rs.set Rs.empty Sds.x (x +. v)]
;;

let sds_estimating = Set.of_list (module Sd.Packed) [ Sd.pack Sds.x ]
let node = Sd_node.create logic sds_estimating
