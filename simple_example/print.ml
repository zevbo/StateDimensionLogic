open Sd_logic
open Core

let logic =
  [%map_open.Sd_lang
    let v = sd Sds.v
    and x = sd Sds.x
    and light_on = sd Sds.light_on in
    printf "v: %f, x: %f, light on?: %b\n" v x light_on;
    Out_channel.flush stdout;
    Rs.empty]
;;

let node = Sd_node.create logic (Set.empty (module Sd.Packed))
