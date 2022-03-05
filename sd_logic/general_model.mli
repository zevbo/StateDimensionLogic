open! Core

type safety = Sd_est.safety
type t

val create : Sd_node.conn list -> 'a Sd_node.t -> t
val run_tick : t -> safety:safety -> t
val run : t -> safety:safety -> num_ticks:int -> t