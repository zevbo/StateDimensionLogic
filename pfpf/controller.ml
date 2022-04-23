open! Core
open! Sd_logic
open! Sd_lang

let est =
  Sd_est.create
    (let+ () = return () in
     let rs = Rs.set Rs.empty State_sds.linput 0.0 in
     Rs.set rs State_sds.rinput 0.0)
    [ Sd.pack State_sds.linput; Sd.pack State_sds.rinput ]
;;
