open! Core
include Applicative.S

type ('a, _) default =
  | V : 'a -> ('a, 'a) default (* in case of too few states, return associated value of type 'a *)
  | Last : ('a, 'a) default (* in case of too few states, use the oldest state *)
  | Safe_last : ('a, 'a option) default (* like last, except in case of too few states and only current state exists, use None *)
  | Unsafe : ('a, 'a) default
  | Op : ('a, 'a option) default
(* in case of too few states, fail *)

exception Sd_not_found of (Sd.Packed.t * int) [@@deriving sexp]

type packed = P : 'a t -> packed

val dependencies_p : packed -> int Map.M(Sd.Packed).t
val dependencies : 'a t -> int Map.M(Sd.Packed).t

val dependency_union
  :  int Map.M(Sd.Packed).t
  -> int Map.M(Sd.Packed).t
  -> int Map.M(Sd.Packed).t

val execute : 'a t -> Rsh.t -> 'a
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
val return : 'a -> 'a t
val sd : 'a Sd.t -> 'a t
val sd_past : 'a Sd.t -> int -> ('a, 'b) default -> 'b t
val sd_history : 'a Sd.t -> int -> (int -> 'a option) t
val state : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> Rs.t t
val state_past : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> int -> Rs.t t
val full_rsh : unit -> Rsh.t t