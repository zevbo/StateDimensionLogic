open! Core

module type Node = sig
  type t [@@deriving hash, compare, sexp_of]
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end

module Graph (N : Node) : sig
  type t [@@deriving sexp_of]

  val empty : t
  val create : Set.M(N).t Map.M(N).t -> t
  val assert_safety : t -> unit
  val as_map : t -> Set.M(N).t Map.M(N).t
  val add_node : t -> N.t -> t
  val add_edge : t -> N.t -> N.t -> t
  val add_edges : t -> N.t -> N.t list -> t
  val remove_edge : t -> N.t -> N.t -> t
  val remove_children : t -> N.t -> t
  val next : t -> N.t -> Set.M(N).t option
  val scc_list : t -> Set.M(N).t list
  val scc_graph : t -> Set.M(N).t Map.M(N).t * t
  val rev : t -> t
  val top_sort : t -> N.t list
end
