open Sd_logic
open Core

let end_cond =
  [%map_open.Sd_lang
    let x = sd Sds.x in
    Float.(x > 100.)]
;;