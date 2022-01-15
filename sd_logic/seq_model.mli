type t

type safety =
  | Safe
  | Warnings
  | Unsafe

val create : ?safety:safety -> Sd_node.t list -> t
val tick : t -> t

(* max_ticks = -1 -> no max *)
val run : ?min_ms:float -> ?max_ticks:int -> ?end_cond:bool Sd_lang.t -> t -> unit
