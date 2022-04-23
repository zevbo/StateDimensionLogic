open! Core
open! Sd_logic

let lencoder = Encoder.create 0.1 State_sds.lpos
let rencoder = Encoder.create 0.1 State_sds.rpos
let gps = Gps.create State_sds.pos 0.1 3. 0.3

let seq_model =
  Seq_model.create
    [ Update_pos.update_omega_est
    ; Update_pos.update_pos_est
    ; Encoder.est lencoder
    ; Encoder.est rencoder
    ; Gps.est gps
    ; Controller.est
    ]
;;

let t = Seq_model.run seq_model ~max_ticks:10