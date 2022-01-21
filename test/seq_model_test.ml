open Sd_logic
open Core

module Sds = struct
  let a = Sd.create "a" Float.sexp_of_t
  let b = Sd.create "b" Float.sexp_of_t
  let bad = Sd.create "bad" Bool.sexp_of_t
end

let set_a_logic =
  [%map_open.Sd_lang
    let () = return () in
    Rs.set Rs.empty Sds.a 0.0]
;;

let bad_set_a_logic =
  [%map_open.Sd_lang
    let _a = sd Sds.a in
    Rs.set Rs.empty Sds.a 0.0]
;;

let set_b_logic =
  [%map_open.Sd_lang
    let a = sd_past Sds.a 0 (V 0.0) in
    Rs.set Rs.empty Sds.b (a +. 1.0)]
;;

let set_b_logic2 =
  [%map_open.Sd_lang
    let a = sd Sds.a in
    Rs.set Rs.empty Sds.b (a +. 1.0)]
;;

let bad_logic =
  [%map_open.Sd_lang
    let _bad = sd Sds.bad in
    Rs.empty]
;;

let extra_set_b_logic =
  [%map_open.Sd_lang
    let () = return () in
    Rs.set (Rs.set Rs.empty Sds.b 0.0) Sds.a 1.0]
;;

let missing_set_b_logic =
  [%map_open.Sd_lang
    let () = return () in
    Rs.empty]
;;

let set_a_node =
  Sd_node.create set_a_logic (Set.of_list (module Sd.Packed) [ Sd.pack Sds.a ])
;;

let bad_set_a =
  Sd_node.create bad_set_a_logic (Set.of_list (module Sd.Packed) [ Sd.pack Sds.a ])
;;

let set_b_node =
  Sd_node.create set_b_logic (Set.of_list (module Sd.Packed) [ Sd.pack Sds.b ])
;;

let set_b_node2 =
  Sd_node.create set_b_logic (Set.of_list (module Sd.Packed) [ Sd.pack Sds.b ])
;;

let bad_node = Sd_node.create bad_logic (Set.of_list (module Sd.Packed) [])

let extra_set_b_node =
  Sd_node.create extra_set_b_logic (Set.of_list (module Sd.Packed) [ Sd.pack Sds.b ])
;;

let missing_set_b_node =
  Sd_node.create missing_set_b_logic (Set.of_list (module Sd.Packed) [ Sd.pack Sds.b ])
;;

let end_cond =
  [%map_open.Sd_lang
    let b = sd Sds.b in
    Float.(b > 1.0)]
;;

let%test "premature_req" =
  try
    ignore (Seq_model.create [ set_b_node; set_a_node ] ~end_cond : Seq_model.t);
    false
  with
  | e ->
    (match e with
    | Seq_model.Premature_sd_req sd -> Sd.Packed.equal sd (Sd.pack Sds.a)
    | _ -> false)
;;

let%test "premature_req2" =
  try
    ignore (Seq_model.create [ set_b_node2; set_a_node ] ~end_cond : Seq_model.t);
    false
  with
  | e ->
    (match e with
    | Seq_model.Premature_sd_req sd -> Sd.Packed.equal sd (Sd.pack Sds.a)
    | _ -> false)
;;

let%test "premature_req3" =
  try
    ignore (Seq_model.create [ bad_set_a ] : Seq_model.t);
    false
  with
  | e ->
    (match e with
    | Seq_model.Premature_sd_req sd -> Sd.Packed.equal sd (Sd.pack Sds.a)
    | _ -> false)
;;

let%test "never_written_req" =
  try
    let model = Seq_model.create [ bad_node; set_a_node; set_b_node ] ~end_cond in
    Seq_model.run ~max_ticks:10 model;
    false
  with
  | e ->
    (match e with
    | Seq_model.Never_written_req sd -> Sd.Packed.equal sd (Sd.pack Sds.bad)
    | _ -> false)
;;

let%test "overwriting_est" =
  try
    ignore (Seq_model.create [ set_a_node; set_a_node ] : Seq_model.t);
    false
  with
  | e ->
    (match e with
    | Seq_model.Overwriting_sd_estimate sd -> Sd.Packed.equal sd (Sd.pack Sds.a)
    | _ -> false)
;;

let%test "extra_est" =
  try
    let model = (Seq_model.create [ extra_set_b_node ] : Seq_model.t) in
    Seq_model.run ~max_ticks:1 model;
    false
  with
  | e ->
    (match e with
    | Sd_node.Extra_sd sd -> Sd.Packed.equal sd (Sd.pack Sds.a)
    | _ -> false)
;;

let%test "missing_est" =
  try
    let model = (Seq_model.create [ missing_set_b_node ] : Seq_model.t) in
    Seq_model.run ~max_ticks:1 model;
    false
  with
  | e ->
    (match e with
    | Sd_node.Missing_sd sd -> Sd.Packed.equal sd (Sd.pack Sds.b)
    | _ -> false)
;;
