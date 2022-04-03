open! Core

module type Node = sig
  type t [@@deriving hash, compare, sexp_of]
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end

module Conn_graph (N : Node) = struct
  type t = (N.t, Set.M(N).t, N.comparator_witness) Map.t

  let a = Int.comparator
  let next (t : t) n = Option.value (Map.find t n) ~default:(Set.empty (module N))

  type _ top_sort_ord_ty =
    | Regular : N.t list top_sort_ord_ty
    | Split : N.t list list top_sort_ord_ty

  let top_sort_ord : type a. t -> N.t list -> a top_sort_ord_ty -> a =
   fun t ord ty ->
    let explored = Hash_set.create (module N) in
    let rec sorter l n =
      if Hash_set.mem explored n
      then l
      else (
        Hash_set.add explored n;
        n :: Set.fold (next t n) ~f:sorter ~init:l)
    in
    match ty with
    | Regular -> List.fold_left ord ~f:sorter ~init:[]
    | Split -> List.fold_left ord ~f:(fun l n -> sorter [] n :: l) ~init:[]
 ;;

  let top_sort t = top_sort_ord t (Set.to_list (Map.key_set t)) Regular

  let scc_list t =
    let first_sort = top_sort t in
    let second_sort = top_sort_ord t (List.rev first_sort) Split in
    List.map second_sort ~f:(Set.of_list (module N))
  ;;
end