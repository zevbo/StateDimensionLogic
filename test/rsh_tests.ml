open! Core
open! Src

module Commons = struct
  include Robot_state_tests.Commons
end

open Commons

let%test_unit "bad_creation" =
  OUnit2.assert_raises
    (Invalid_argument
       "~max_length in Robot_state_history.create must be positive. Given 0")
    (fun () -> Rsh.create ~max_length:0)
;;

let%expect_test "empty" =
  print_s (Rsh.sexp_of_t (Rsh.create ~max_length:10));
  [%expect {| ((states (((data ()) (sd_map <opaque>)))) (max_length 10)) |}]
;;

let%expect_test "use" =
  let rsh = Rsh.create ~max_length:2 in
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
  let rsh1 = Rsh.create ~max_length:2 in
  let rsh2 = Rsh.create ~max_length:5 in
  let rsh1 = Rsh.add_state rsh1 in
  let rsh1 = Rsh.add_state rsh1 in
  let rsh1 = Rsh.add_state rsh1 in
  let rsh2 = Rsh.add_state rsh2 in
  let rsh2 = Rsh.add_state rsh2 in
  let rsh2 = Rsh.add_state rsh2 in
  print_endline (Int.to_string (Rsh.length rsh1));
  print_endline (Int.to_string (Rsh.length rsh2));
  [%expect {|
    2
    4 |}]
;;

let%test "max_len1" =
  let rsh = Rsh.create ~max_length:1 in
  let rsh = Rsh.add_state rsh in
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
        ~f:(fun state rsh -> Rsh.use (Rsh.add_state rsh) state)
        ~init:(Rsh.create ~max_length:num_states)
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
