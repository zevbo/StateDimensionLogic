open! Core
open! Sd_logic

let model = Seq_model.create [ Update_v.node; Update_x.node; Light_on.node; Print.node ]
let run () = Seq_model.run model ~ticks:(Some 100)
