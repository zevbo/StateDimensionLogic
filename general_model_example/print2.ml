open! Sd_logic
open! Core

let logic =
  [%map_open.Sd_lang
    let () = return () in
    print_endline "Starting...";
    Thread.delay 0.005;
    print_endline "Done";
    Rs.empty]
;;

let node = Sd_node.est (Sd_est.create logic (Set.empty (module Sd.Packed)))
