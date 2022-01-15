open! Core
open! Sd_logic

let model = Seq_model.create [ Update_v.node; Update_x.node; Light_on.node; Print.node ]

let simple_end_cond =
  [%map_open.Sd_lang
    let x = sd Sds.x in
    Float.(x > 100.)]
;;

let run () =
  Seq_model.run model ~max_ticks:100;
  Seq_model.run model ~end_cond:simple_end_cond
;;
