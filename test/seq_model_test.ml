open Sd_logic
open Core
open Sd_func

module Sds = struct
  let a = Sd.create "a" Float.sexp_of_t
  let b = Sd.create "b" Float.sexp_of_t
  let bad = Sd.create "bad" Bool.sexp_of_t
end

let set_a_logic =
  let+ () = return () in
  Rs.set Rs.empty Sds.a 0.0
;;

let bad_set_a_logic =
  let+ _a = sd Sds.a in
  Rs.set Rs.empty Sds.a 0.0
;;

let set_b_logic =
  let+ a = sd_past Sds.a 0 (V 0.0) in
  Rs.set Rs.empty Sds.b (a +. 1.0)
;;

let set_b_logic2 =
  let+ a = sd Sds.a in
  Rs.set Rs.empty Sds.b (a +. 1.0)
;;

let bad_logic =
  let+ _bad = sd Sds.bad in
  Rs.empty
;;

let extra_set_b_logic =
  let+ () = return () in
  Rs.set (Rs.set Rs.empty Sds.b 0.0) Sds.a 1.0
;;

let missing_set_b_logic =
  let+ () = return () in
  Rs.empty
;;

let set_a_node = Sd_est.create set_a_logic [ Sd_est.sd Sds.a ]
let bad_set_a = Sd_est.create bad_set_a_logic [ Sd_est.sd Sds.a ]
let set_b_node = Sd_est.create set_b_logic [ Sd_est.sd Sds.b ]
let set_b_node2 = Sd_est.create set_b_logic [ Sd_est.sd Sds.b ]
let bad_node = Sd_est.create bad_logic []
let extra_set_b_node = Sd_est.create extra_set_b_logic [ Sd_est.sd Sds.b ]
let missing_set_b_node = Sd_est.create missing_set_b_logic [ Sd_est.sd Sds.b ]

let end_cond =
  let+ b = sd Sds.b in
  Float.(b > 1.0)
;;

let check_error ?end_cond nodes to_run =
  let model_pass = Seq_model.create ?end_cond nodes in
  if to_run then Seq_model.run ~max_ticks:1 model_pass;
  try
    let model = Seq_model.create nodes in
    if to_run then Seq_model.run ~max_ticks:1 model
  with
  | e -> print_string (Exn.to_string e)
;;

(*
let%expect_test "premature_req" =
  check_error
    [ set_b_node; set_a_node ]
    (Seq_model.create_safety ~premature_sd_req:Unsafe ())
    false;
  [%expect {| (sd_logic/seq_model.ml.Premature_sd_req a) |}]
;;

let%expect_test "premature_req2" =
  check_error
    [ set_b_node2; set_a_node ]
    (Seq_model.create_safety ~premature_sd_req:Unsafe ())
    false;
  [%expect {| (sd_logic/seq_model.ml.Premature_sd_req a) |}]
;;

let%expect_test "premature_req3" =
  check_error [ bad_set_a ] (Seq_model.create_safety ~premature_sd_req:Unsafe ()) false;
  [%expect {| (sd_logic/seq_model.ml.Premature_sd_req a) |}]
;;

let%expect_test "never_written_req" =
  check_error
    [ bad_node; set_a_node; set_b_node ]
    (Seq_model.create_safety ~never_written_sd_req:Unsafe ())
    false;
  [%expect {| (sd_logic/seq_model.ml.Never_written_req bad) |}]
;;

let%expect_test "overwriting_est" =
  check_error
    [ set_a_node; set_a_node ]
    (Seq_model.create_safety ~overwritten_sd:Unsafe ())
    false;
  [%expect {| (sd_logic/seq_model.ml.Overwriting_sd_estimate a) |}]
;;

let%expect_test "extra_est" =
  let node_safety = Sd_est.create_safety ~extra_sd:Unsafe () in
  check_error [ extra_set_b_node ] (Seq_model.create_safety ~node_safety ()) true;
  [%expect {| (sd_logic/sd_est.ml.Extra_sd a) |}]
;;

let%expect_test "missing_est" =
  let node_safety = Sd_est.create_safety ~missing_sd:Unsafe () in
  check_error [ missing_set_b_node ] (Seq_model.create_safety ~node_safety ()) true;
  [%expect {| (sd_logic/sd_est.ml.Missing_sd b) |}]
;;
*)
