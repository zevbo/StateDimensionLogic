open! Sd_logic
open Simple_example

let logic =
  [%map_open.Sd_lang
    let x = sd Sds.x
    and v = sd Sds.v in
    print_endline "In slow_desc";
    x +. (10. *. v) > 100.0]
;;

let node = Sd_node.desc logic