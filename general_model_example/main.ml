open! Core
open! Sd_logic
open Simple_example

let update_v_node = Sd_node.est Update_v.est
let update_x_node = Sd_node.est Update_x.est
let light_on_node = Sd_node.est Light_on.est
let print_node = Sd_node.est Print.est
let end_fork = Sd_node.fork ()
let main_tick = Sd_node.tick ()

(* things I don't like about this: 
  - overhead
  - if you want to open Sd_node like I have here you can't use identifiers like fork or tick  
  *)
let connections =
  Sd_node.
    [ Conn (Slow_desc.node, (P Update_v_slow.node, P update_v_node))
    ; Conn (update_v_node, P update_x_node)
    ; Conn (Update_v_slow.node, P update_x_node)
    ; Conn (update_x_node, P light_on_node)
    ; Conn (light_on_node, P end_fork)
    ; Conn (end_fork, (P Print2.node, P print_node))
    ; Conn (print_node, P main_tick)
    ; Conn (main_tick, P Slow_desc.node)
    ; Conn (Print2.node, P Sd_node.exit)
    ]
;;

let model = General_model.create connections Slow_desc.node

let run () =
  let _model =
    General_model.run model ~safety:(Sd_est.create_safety ~default:Safe ()) ~num_ticks:80
  in
  ()
;;
