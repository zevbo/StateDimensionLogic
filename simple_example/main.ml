open! Core
open! Sd_logic

let model = Seq_model.create [ Update_v.est; Update_x.est; Light_on.est; Print.est ]
let run () = Seq_model.run model ~ticks:(Some 100)
