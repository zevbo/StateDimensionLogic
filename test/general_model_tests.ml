open! Sd_logic
open! General_model_example
open Sd_lang

module Commons = struct
  include Robot_state_tests.Commons
end

let tick1 = Sd_node.tick ()
let tick2 = Sd_node.tick ()
let fork = Sd_node.fork ()

let est_logic =
  let+ a = sd_past Commons.a 1 (V 0.0) in
  Rs.set Rs.empty Commons.a (a +. 1.0)
;;

let est = Sd_node.estl est_logic [ Sd.pack Commons.a ]

let%test "exponential1" =
  try
    let _r =
      Gm.create
        [ Conn (tick1, C fork); Conn (fork, (C tick1, C tick2)); Conn (tick2, C tick1) ]
        tick1
    in
    false
  with
  | Gm.Possible_exponential_threading _id -> true
  | _ -> false
;;

let%test "possible_overwrte" =
  try
    let _r =
      Gm.create
        [ Conn (tick1, C est); Conn (est, C fork); Conn (fork, (C est, C tick1)) ]
        tick1
    in
    false
  with
  | Gm.Possible_overwrite sd -> Sd.Packed.equal sd (Sd.pack Commons.a)
  | _ -> false
;;
