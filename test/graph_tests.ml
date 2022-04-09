open! Sd_logic
open Core
module Graph = Graph.Graph (Int)

let sol = Set.of_list (module Int)
let g_map = Map.of_alist_exn (module Int) [ 1, sol [ 2; 3 ]; 2, sol [ 3 ] ]
let g_map2 = Map.of_alist_exn (module Int) [ 1, sol [ 2; 3 ]; 3, sol [ 1; 4 ] ]
let g = Graph.create g_map
let g2 = Graph.create g_map2
let rev = Graph.rev g

let%test "rev" =
  Map.equal
    Set.equal
    (Graph.as_map rev)
    (Map.of_alist_exn (module Int) [ 3, sol [ 1; 2 ]; 2, sol [ 1 ] ])
;;

let top_sort1 = Graph.top_sort g
let top_sort_rev1 = Graph.top_sort rev

let%expect_test "top_sort1" =
  print_s (List.sexp_of_t Int.sexp_of_t top_sort1);
  [%expect {| (1 2 3) |}]
;;

let%expect_test "top_sort_rev1" =
  print_s (List.sexp_of_t Int.sexp_of_t top_sort_rev1);
  [%expect {| (3 2 1) |}]
;;

let scc_list1 = Graph.scc_list g
let scc_list2 = Graph.scc_list g2

let%expect_test "scc1" =
  let sexp = List.sexp_of_t (Set.sexp_of_m__t (module Int)) scc_list1 in
  print_s sexp;
  [%expect {| ((3) (2) (1)) |}]
;;

let%expect_test "scc2" =
  let sexp = List.sexp_of_t (Set.sexp_of_m__t (module Int)) scc_list2 in
  print_s sexp;
  [%expect {| ((2) (4) (1 3)) |}]
;;
