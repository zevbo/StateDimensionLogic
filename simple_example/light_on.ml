open Core
open Sd_logic

let logic =
  [%map_open.Sd_lang
    let x = sd Sds.x in
    Rs.set Rs.empty Sds.light_on Float.(x > 50.0)]
;;

let sds_estimating = Set.of_list (module Sd.Packed) [ Sd.pack Sds.light_on ]
let node = Sd_node.create logic sds_estimating
