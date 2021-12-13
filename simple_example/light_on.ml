open Core
open Src
open State_estimators

let logic =
  [%map_open.Sd_lang
    let x = sd Sds.x in
    Rs.set Rs.empty Sds.light_on Float.(x > 50.0)]
;;

let est = Est.create logic (Set.of_list (module Sd.Packed) [ Sd.pack Sds.light_on ])
