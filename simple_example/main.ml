open! Core
open! Sd_logic

let model =
  Seq_model.create
    [ Update_v.node; Update_x.node; Light_on.node; Print.node ]
    ~end_cond:End_cond.end_cond
;;

let run () =
  Seq_model.run model ~no_end_cond:true ~max_ticks:100;
  Seq_model.run model
;;