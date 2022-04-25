open! Core
open! Sd_logic
open! Sensors
open! Sd_lang

let print_error =
  let logic =
    let+ pos = sd State_sds.pos
    and+ mpos = sd Mupdate_pos.mpos_sd
    and+ angle = sd State_sds.angle
    and+ mangle = sd Mupdate_pos.mangle_sd in
    let diff = Vec.sub pos mpos in
    Printf.printf "Angles: %f, %f\n" angle mangle;
    Printf.printf "Dif: %f [out of %f]\n" (Vec.mag diff) (Vec.mag pos);
    Rs.empty
  in
  Sd_est.create logic []
;;

let seq_model =
  Seq_model.create
    [ Update_pos.update_omega_est
    ; Update_pos.update_pos_est
    ; Encoder.est lencoder
    ; Encoder.est rencoder
    ; Gps.est gps
    ; Imu.est imu (* ; Mupdate_pos.est *)
    ; Pf_update.pf
    ; Controller.est
    ; print_error
    ]
;;

let t = Seq_model.run seq_model ~max_ticks:400