open Core
open Sd_logic
open Sd_func

let logic =
  let+ x = sd Sds.x in
  Printf.printf "executing light_on logic\n";
  Rs.set Rs.empty Sds.light_on Float.(x > 50.0)
;;

let est =
  Sd_est.create_reactive
    logic
    [ Sd_est.Equal_sd.E (Sds.light_on, Bool.equal) ]
    ~signal:false
;;
