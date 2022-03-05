open! Sd_logic
open Simple_example

let logic =
  [%map_open.Sd_lang
    let x = sd_past Sds.x 1 (V 0.0)
    and v = sd_past Sds.v 1 (V 0.0) in
    x +. (10. *. v) > 100.0]
;;

let node = Sd_node.desc logic