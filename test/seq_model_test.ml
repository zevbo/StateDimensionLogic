open Simple_example
open Sd_logic
open Core

module Sds = struct
  let a = Sd.create "a" Float.sexp_of_t
  let b = Sd.create "b" Float.sexp_of_t
  let c = Sd.create "c" Bool.sexp_of_t
end

let logic =
  [%map_open.Sd_lang
    let x = sd_past Sds.x 1 (V 0.0)
    and v = sd Sds.v in
    Rs.set Rs.empty Sds.x (x +. v)]
;;

let sds_estimating = Set.of_list (module Sd.Packed) [ Sd.pack Sds.x ]
let node = Sd_node.create logic sds_estimating
