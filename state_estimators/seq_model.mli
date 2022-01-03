type t

type safety =
  | Safe
  | Warnings
  | Unsafe

val create : ?safety:safety -> Est.t list -> t
val tick : t -> t
val run : ?min_ms:float -> t -> ticks:int option -> unit
