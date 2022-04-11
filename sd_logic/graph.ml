open! Core

module type Node = sig
  type t [@@deriving hash, compare, sexp_of]
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end

module Graph (N : Node) = struct
  type t = Set.M(N).t Map.M(N).t [@@deriving sexp_of]

  let empty : t = Map.empty (module N)
  let create map : t = map
  let as_map t = t
  let next_w_def t n = Option.value (Map.find t n) ~default:(Set.empty (module N))
  let add_node t n = Map.set t ~key:n ~data:(next_w_def t n)
  let add_edge t n1 n2 = Map.set t ~key:n1 ~data:(Set.add (next_w_def t n1) n2)

  let add_edges t n ns =
    if List.is_empty ns && not (Map.mem t n)
    then add_node t n
    else List.fold_left ns ~init:t ~f:(fun t child_n -> add_edge t n child_n)
  ;;

  let remove_edge t n1 n2 =
    let curr = next_w_def t n1 in
    Map.set t ~key:n1 ~data:(Set.remove curr n2)
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
    let second_sort = top_sort_ord (rev t) first_sort Split in
    List.map second_sort ~f:(Set.of_list (module N))
  ;;

  let scc_graph t =
    let l = scc_list t in
    let sccs_and_ids = List.map l ~f:(fun a -> Option.value_exn (Set.min_elt a), a) in
    let node_to_id =
      List.fold_left
        sccs_and_ids
        ~init:(Map.empty (module N))
        ~f:(fun m (id, scc) ->
          Set.fold scc ~init:m ~f:(fun m id_in -> Map.set m ~key:id_in ~data:id))
    in
    let g =
      List.fold_left sccs_and_ids ~init:empty ~f:(fun g (id, scc) ->
          let old_out_edges =
            Set.fold
              scc
              ~init:(Set.empty (module N))
              ~f:(fun too_add sc -> Set.union too_add (next t sc))
          in
          let scc_out_edges =
            Set.map (module N) old_out_edges ~f:(fun node -> Map.find_exn node_to_id node)
          in
          let scc_out_edges = Set.remove scc_out_edges id in
          add_edges g id (Set.to_list scc_out_edges))
    in
    Map.of_alist_exn (module N) sccs_and_ids, g
  ;;
end