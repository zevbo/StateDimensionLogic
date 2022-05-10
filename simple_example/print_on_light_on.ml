open Core
open Sd_logic
open Sd_func

let logic =
  let+ light_on = sd Sds.light_on in
  Printf.printf "In light on check logic... ";
  if light_on then Printf.printf "Light is now on!";
  Printf.printf "\n";
  Rs.empty
;;

let est = Sd_est.create ~signal:false logic []