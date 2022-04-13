open! Core

type safety = Sd_est.safety
type t

val create : Sd_node.conn list -> ('a, 'b) Sd_node.t -> t
val run_tick : t -> safety:safety -> t
val run : t -> safety:safety -> num_ticks:int -> t
val rsh : t -> Rsh.t

exception Unsafe_curr_requirement of Sd.Packed.t [@@deriving sexp]

(* when inconsitent estimates are introduced, will need a new error *)
exception Possible_overwrite of Sd.Packed.t [@@deriving sexp]
exception Possible_exponential_threading of Sd_node.child_t
exception Unconnected_node of Sd_node.child_t
exception Infinite_loop
