open! Core
open! Sd_logic

let model =
  Seq_model.create
    [ Update_v.est; Update_x.est; Light_on.est; Print_on_light_on.est; Print.est ]
    ~end_cond:End_cond.end_cond
;;

let run () =
  (* Seq_model.run model ~no_end_cond:true ~max_ticks:100; *)
  Seq_model.run model
;;
