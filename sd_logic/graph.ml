open! Core

module type Node = sig
  type t [@@deriving hash, compare, sexp_of]
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end

module Graph (N : Node) = struct
  type t = Set.M(N).t Map.M(N).t

  let empty : t = Map.empty (module N)
  let create map : t = map
  let as_map t = t

  let add_edge t n1 n2 =
    let prev = Option.value (Map.find t n1) ~default:(Set.empty (module N)) in
    Map.set t ~key:n1 ~data:(Set.add prev n2)
  ;;

  let add_edges t n ns =
    List.fold_left ns ~init:t ~f:(fun t child_n -> add_edge t n child_n)
  ;;

  let next (t : t) n = Option.value (Map.find t n) ~default:(Set.empty (module N))

  let rev (t : t) =
    Map.fold t ~init:empty ~f:(fun ~key ~data t ->
        Set.fold data ~init:t ~f:(fun t n -> add_edge t n key))
  ;;

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
    | Split ->
      List.fold_left
        ord
        ~f:(fun l n ->
          match sorter [] n with
          | [] -> l
          | r -> r :: l)
        ~init:[]
 ;;

  let top_sort t = top_sort_ord t (Set.to_list (Map.key_set t)) Regular

  let scc_list t =
    let first_sort = top_sort t in
    let second_sort = top_sort_ord (rev t) (List.rev first_sort) Split in
    List.map second_sort ~f:(Set.of_list (module N))
  ;;
end