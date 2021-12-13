open Core
open Src
open State_estimators

let logic =
  [%map_open.Sd_lang
    let v = sd_past Sds.v 1 (V 0.0) in
    let diff = 0.1 +. Random.float_range (-0.5) 0.5 in
    Rs.set Rs.empty Sds.v (v +. diff)]
;;

let sds_estiamting = Set.of_list (module Sd.Packed) [ Sd.pack Sds.v ]
let est = Est.create logic sds_estiamting
