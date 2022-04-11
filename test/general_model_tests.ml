open! Sd_logic
open! General_model_example
open Sd_lang

module Commons = struct
  include Robot_state_tests.Commons
end

let tick1 = Sd_node.tick ()
let tick2 = Sd_node.tick ()
let fork = Sd_node.fork ()
let fork2 = Sd_node.fork ()

let est_logic =
  let+ a = sd_past Commons.a 1 (V 0.0) in
  Rs.set Rs.empty Commons.a (a +. 1.0)
;;

let est = Sd_node.estl est_logic [ Sd.pack Commons.a ]

let est2_logic =
  let+ _a = sd Commons.a in
  Rs.empty
;;

let est2 = Sd_node.estl est2_logic []

let nothing_est_logic =
  let+ () = return () in
  Rs.empty
;;

let nothing_est1 = Sd_node.estl nothing_est_logic []
let nothing_est2 = Sd_node.estl nothing_est_logic []
let nothing_est3 = Sd_node.estl nothing_est_logic []

let nothing_desc =
  Sd_node.desc
    (let+ () = return () in
     false)
;;

let nothing_desc2 =
  Sd_node.desc
    (let+ () = return () in
     false)
;;

let%test "okay1" =
  let _r =
    Gm.create
      [ Conn (tick1, C est)
      ; Conn (est, C est2)
      ; Conn (est2, C nothing_desc)
      ; Conn (nothing_desc, (C est2, C tick1))
      ]
      tick1
  in
  true
;;

let%test "exponential1" =
  try
    let _r =
      Gm.create
        [ Conn (tick1, C fork); Conn (fork, (C tick1, C tick2)); Conn (tick2, C tick1) ]
        tick1
    in
    false
  with
  | Gm.Possible_exponential_threading _node -> true
  | _ -> false
;;

let%test "exponential2" =
  try
    let _r =
      Gm.create
        [ Conn (tick1, C fork)
        ; Conn (fork, (C nothing_desc, C tick1))
        ; Conn (nothing_desc, (C fork, C Sd_node.exit))
        ]
        tick1
    in
    false
  with
  | Gm.Possible_exponential_threading __nodeid -> true
  | _ -> false
;;

let%test "infinite-loop" =
  try
    let _r = Gm.create [ Conn (tick1, C est); Conn (est, C est) ] tick1 in
    false
  with
  | Gm.Infinite_loop -> true
  | _ -> false
;;

let%test "infinite-loop2" =
  try
    let _r =
      Gm.create
        [ Conn (tick1, C fork); Conn (fork, (C est, C fork)); Conn (est, C fork) ]
        tick1
    in
    false
  with
  | Gm.Infinite_loop -> true
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
  | e -> raise e
;;

let%test "okay2" =
  let _r =
    Gm.create
      [ Conn (tick1, C est)
      ; Conn (est, C nothing_est1)
      ; Conn (nothing_est1, C nothing_est2)
      ; Conn (nothing_est2, C nothing_est3)
      ; Conn (nothing_est3, C est2)
      ; Conn (est2, C tick1)
      ]
      tick1
  in
  true
;;

let%test "okay3" =
  let _r =
    Gm.create
      [ Conn (tick1, C est)
      ; Conn (est, C nothing_desc)
      ; Conn (nothing_desc, (C est2, C nothing_est1))
      ; Conn (est2, C nothing_est1)
      ; Conn (nothing_est1, C nothing_est2)
      ; Conn (nothing_est2, C nothing_desc2)
      ; Conn (nothing_desc2, (C est2, C tick1))
      ]
      tick1
  in
  true
;;
