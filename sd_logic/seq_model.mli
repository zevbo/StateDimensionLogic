type t

type safety =
  | Safe
  | Warnings
  | Unsafe

val create : ?safety:safety -> ?end_cond:bool Sd_lang.t -> Sd_node.t list -> t
val tick : t -> t

(* max_ticks = -1 -> no max *)
val run : ?no_end_cond:bool -> ?min_ms:float -> ?max_ticks:int -> t -> unit