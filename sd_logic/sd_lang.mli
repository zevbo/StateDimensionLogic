open! Core
include Applicative.S

type 'a default =
  | V of 'a (* in case of too few states, return associated value of type 'a *)
  | Last (* in case of too few states, use the oldest state *)
  | Safe_last of 'a (* like last, except in case of too few states and only current state exists, use 'a *)
  | Unsafe
(* in case of too few states, fail *)

exception Sd_not_found of (string * int)

type packed = P : 'a t -> packed

val dependencies_p : packed -> int Map.M(Sd.Packed).t
val dependencies : 'a t -> int Map.M(Sd.Packed).t
val execute : 'a t -> Rsh.t -> 'a

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
      val state : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> Rs.t t
      val state_past : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> int -> Rs.t t
      val full_rsh : unit -> Rsh.t t
    end
  end
end