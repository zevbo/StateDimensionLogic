open! Core
open! Src
include Applicative.S

type 'a default =
  | Last
  | V of 'a

val dependencies : 'a t -> (Sd.Packed.t, int, Sd.Packed.comparator_witness) Map.t
val execute : Rsh.t -> 'a t -> 'a

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig
      val return : 'a -> 'a t
      val sd : 'a Sd.t -> 'a t
      val sd_past : 'a Sd.t -> int -> 'a default -> 'a t
      val sd_history : 'a Sd.t -> int -> (int -> 'a option) t
      val full_rsh : unit -> Rsh.t t
    end
  end
end
