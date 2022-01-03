open! Core
open! Src
open! State_estimators

let print =
  [%map_open.Sd_lang
    let v = sd Sds.v
    and x = sd Sds.x
    and light_on = sd Sds.light_on in
    printf "v: %f, x: %f, light on?: %b\n" v x light_on;
    Out_channel.flush stdout;
    Rs.empty]
;;

let print_est = Est.create print (Set.empty (module Sd.Packed))
let model = Seq_model.create [ Update_v.est; Update_x.est; Light_on.est; print_est ]
let run () = Seq_model.run model ~ticks:(Some 100)
