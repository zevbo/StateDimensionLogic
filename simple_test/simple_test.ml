open! Core
open! State_estimators

let model = Seq_model.create [ Update_v.est; Update_x.est ]

let execution =
  [%map_open.Sd_lang
    let v = sd Sds.v
    and x = sd Sds.x in
    printf "v: %f, x: %f\n" v x]
;;

module Rprogram = Rprogram.M (Seq_model)

let program = Rprogram.create model execution
let ticks = Some 100
let () = Rprogram.run program ~ticks
