open! Sd_logic
open! Core
open Sd_lang

let logic =
  let+ () = return () in
  print_endline "Starting...";
  Thread.delay 0.005;
  print_endline "Done";
  Rs.empty
;;

let node = Sd_node.est (Sd_est.create logic [])
