open! Core
open! Sd_logic
open! Sensors

let seq_model =
  Seq_model.create
    [ (* Update_pos.update_omega_est
    ; Update_pos.update_pos_est
    ; Encoder.est lencoder
    ; Encoder.est rencoder
    ; Gps.est gps
    ; Imu.est imu
    ; Mupdate_pos.est
    ; Controller.est *)
      Imu.est imu
    ]
;;

let t = Seq_model.run seq_model ~max_ticks:10