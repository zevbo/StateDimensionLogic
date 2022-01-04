open! Core
open Sd_logic

module Commons = struct
  type sample =
    | A
    | B
    | C
  [@@deriving sexp, compare]

  let x = Sd.create "x" Float.sexp_of_t
  let y = Sd.create "y" Float.sexp_of_t
  let z = Sd.create "z" Float.sexp_of_t
  let a = Sd.create "a" Float.sexp_of_t
  let b = Sd.create "b" Float.sexp_of_t
  let c = Sd.create "c" Float.sexp_of_t

  let state_w_xyz =
    let state = Rs.set Rs.empty x 10.0 in
    let state = Rs.set state y 0.0 in
    Rs.set state z 100.0
  ;;

  let build_random sds random =
    let sds = List.filter sds ~f:(fun _ -> Random.bool ()) in
    List.fold_left sds ~f:(fun state sd -> Rs.set state sd (random ())) ~init:Rs.empty
  ;;

  let std_sds = [ x; y; z; a; b; c ]
  let build_random_std () = build_random std_sds (fun () -> Random.float_range (-1.0) 1.0)
end

open Commons

let%test "simple_set_and_get" =
  let sd = Sd.create "test" Float.sexp_of_t in
  let state = Rs.set Rs.empty sd 10.0 in
  Float.(Rs.find_exn state sd = 10.0)
;;

let%test_unit "typed_set_and_get" =
  [%test_eq: sample]
    (let state = Rs.empty in
     let sd = Sd.create "sample" sexp_of_sample in
     let state = Rs.set state sd B in
     Rs.find_exn state sd)
    B
;;

let%expect_test "keys" =
  print_s (Set.sexp_of_m__t (module Sd.Packed) (Rs.keys state_w_xyz));
  [%expect {| (x y z) |}]
;;

let%expect_test "simple_remove" =
  let state = Rs.empty in
  let yaw = Sd.create "yaw" Float.sexp_of_t in
  let state = Rs.set state yaw 10.0 in
  let state = Rs.remove state yaw in
  print_s (Rs.sexp_of_t state);
  [%expect {| ((data ()) (sd_map <opaque>)) |}]
;;

let%expect_test "trim_to" =
  let state =
    Rs.trim_to state_w_xyz (Set.of_list (module Sd.Packed) [ Sd.pack x; Sd.pack y ])
  in
  print_s (Rs.sexp_of_t state);
  [%expect {| ((data ((x 10) (y 0))) (sd_map <opaque>)) |}]
;;

let%test "mem" =
  let state = Rs.set Rs.empty x 10.0 in
  let state = Rs.set state y 0.0 in
  let state = Rs.remove state y in
  Rs.mem state x && (not (Rs.mem state y)) && not (Rs.mem state z)
;;

let%test "memp" =
  let state = Rs.set Rs.empty x 10.0 in
  let state = Rs.set state y 0.0 in
  let state = Rs.removep state (P y) in
  Rs.memp state (Sd.pack x)
  && (not (Rs.memp state (Sd.pack y)))
  && not (Rs.memp state (Sd.pack z))
;;

let%expect_test "use" =
  let state1 = Rs.set Rs.empty x 0.0 in
  let state1 = Rs.set state1 y 0.0 in
  let state2 = Rs.set Rs.empty y 10.0 in
  let state2 = Rs.set state2 z 10.0 in
  print_s (Rs.sexp_of_t (Rs.use state1 state2));
  [%expect {| ((data ((x 0) (y 10) (z 10))) (sd_map <opaque>)) |}]
;;

let%expect_test "use2" =
  let state1 = Rs.set Rs.empty x 0.0 in
  let state1 = Rs.set state1 y 0.0 in
  let state1 = Rs.set state1 a 0.0 in
  let state1 = Rs.set state1 b 0.0 in
  let state2 = Rs.set Rs.empty y 10.0 in
  let state2 = Rs.set state2 z 10.0 in
  let state2 = Rs.set state2 b 10.0 in
  let state2 = Rs.set state2 c 10.0 in
  print_s
    (Rs.sexp_of_t
       (Rs.use
          state1
          ~to_use:(Some (Set.of_list (module Sd.Packed) [ P x; P y; P z ]))
          state2));
  [%expect {| ((data ((a 0) (b 0) (x 0) (y 10) (z 10))) (sd_map <opaque>)) |}]
;;