open! Core
open! Src

module Commons = struct
  include Robot_state_tests.Commons
end

open Commons

let%test_unit "bad_creation" =
  OUnit2.assert_raises
    (Invalid_argument
       "~default_length in Robot_state_history.create must be positive. Given 0")
    (fun () -> Rsh.create ~default_length:0 ())
;;

let%expect_test "empty" =
  print_s (Rsh.sexp_of_t (Rsh.create ~default_length:10 ()));
  [%expect {| ((states (((data ()) (sd_map <opaque>)))) (max_length 10)) |}]
;;

let%expect_test "use" =
  let rsh = Rsh.create ~default_length:2 () in
  let to_use = Rs.set Rs.empty x 1.0 in
  let to_use = Rs.set to_use y 1.0 in
  let to_use2 = Rs.set Rs.empty y 2.0 in
  let to_use2 = Rs.set to_use2 z 2.0 in
  let rsh = Rsh.use rsh to_use in
  let rsh = Rsh.use rsh to_use2 in
  print_s (Rsh.sexp_of_t rsh);
  [%expect
    {| ((states (((data ((x 1) (y 2) (z 2))) (sd_map <opaque>)))) (max_length 2)) |}]
;;

let%expect_test "length" =
  let rsh1 = Rsh.create ~default_length:2 () in
  let rsh2 = Rsh.create ~default_length:5 () in
  let rsh1 = Rsh.add_empty_state rsh1 in
  let rsh1 = Rsh.add_empty_state rsh1 in
  let rsh1 = Rsh.add_empty_state rsh1 in
  let rsh2 = Rsh.add_empty_state rsh2 in
  let rsh2 = Rsh.add_empty_state rsh2 in
  let rsh2 = Rsh.add_empty_state rsh2 in
  print_endline (Int.to_string (Rsh.length rsh1));
  print_endline (Int.to_string (Rsh.length rsh2));
  [%expect {|
    2
    4 |}]
;;

let%test "max_len1" =
  let rsh = Rsh.create ~default_length:1 () in
  let rsh = Rsh.add_empty_state rsh in
  Rsh.length rsh = 1
;;

let%test "randomized_tests: find_past, mem_past, memp_past" =
  let num_states = 20 in
  let num_tests = 100 in
  let test () =
    let states = List.init num_states ~f:(fun _ -> build_random_std ()) in
    let rsh =
      List.fold_right
        states
        ~f:(fun state rsh -> Rsh.use (Rsh.add_empty_state rsh) state)
        ~init:(Rsh.create ~default_length:num_states ())
    in
    let check_state (state, i) =
      List.for_all std_sds ~f:(fun sd ->
          let find_past =
            match Rs.find state sd, Rsh.find_past rsh i sd with
            | None, None -> true
            | Some v1, Some v2 -> Float.(v1 = v2)
            | _ -> false
          in
          let mem_past =
            Bool.equal (Rs.mem state sd) (Option.value_exn (Rsh.mem_past rsh i sd))
          in
          let memp_past =
            Bool.equal (Rs.mem state sd) (Option.value_exn (Rsh.memp_past rsh i (P sd)))
          in
          find_past && mem_past && memp_past)
    in
    List.for_all (List.zip_exn states (List.range 0 num_states)) ~f:check_state
  in
  List.for_all (List.init num_tests ~f:(fun _ -> test ())) ~f:ident
;;

let%expect_test "sd_lengths" =
  let sd_lengths =
    Map.of_alist_exn (module Sd.Packed) [ Sd.pack x, 5; Sd.pack z, 1; Sd.pack a, 2 ]
  in
  let rsh = Rsh.create ~sd_lengths () in
  let state = Rs.set Rs.empty x 10.0 in
  let state = Rs.set state y 1.0 in
  let state = Rs.set state z 0.0 in
  let state = Rs.set state a 0.0 in
  let rsh =
    List.fold_left (List.range 0 20) ~f:(fun rsh _ -> Rsh.add_state rsh state) ~init:rsh
  in
  print_s (Rsh.sexp_of_t rsh);
  [%expect
    {|
    ((states
      (((data ((a 0) (x 10) (y 1) (z 0))) (sd_map <opaque>))
       ((data ((a 0) (x 10) (y 1))) (sd_map <opaque>))
       ((data ((a 0) (x 10) (y 1))) (sd_map <opaque>))
       ((data ((x 10) (y 1))) (sd_map <opaque>))
       ((data ((x 10) (y 1))) (sd_map <opaque>))))
     (max_length 5)) |}]
;;

let%test_unit "real_store_len1" = [%test_eq: int] (Rsh.real_store_len 1) 1
let%test_unit "real_store_len2" = [%test_eq: int] (Rsh.real_store_len 2) 3
let%test_unit "real_store_len3" = [%test_eq: int] (Rsh.real_store_len 3) 3
let%test_unit "real_store_len4" = [%test_eq: int] (Rsh.real_store_len 4) 7

let%test "randomized:sd_lengths" =
  let max_len = 40 in
  let real_max_len = max_len * 2 in
  let sd_lengths =
    Map.of_alist_exn
      (module Sd.Packed)
      (List.map std_sds ~f:(fun sd -> Sd.pack sd, 1 + Random.int max_len))
  in
  let state = List.fold std_sds ~f:(fun rs sd -> Rs.set rs sd 0.0) ~init:Rs.empty in
  let rsh = Rsh.create ~default_length:real_max_len ~sd_lengths () in
  let rsh =
    List.fold_left
      (List.range 0 real_max_len)
      ~f:(fun rsh _ -> Rsh.add_state rsh state)
      ~init:rsh
  in
  Map.for_alli sd_lengths ~f:(fun ~key ~data ->
      let i = Rsh.real_store_len data - 1 in
      Option.value_exn (Rsh.memp_past rsh i key)
      && not (Option.value_exn (Rsh.memp_past rsh (i + 1) key)))
;;
