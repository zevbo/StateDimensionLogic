open! Core
include Applicative.S

type ('a, _) default =
  | V : 'a -> ('a, 'a) default (* in case of too few states, return associated value of type 'a *)
  | Safe_last : ('a, 'a option) default (* in case of too few states, use the oldest state. in case of too few states and only current state exists, use None *)
  | Op : ('a, 'a option) default

exception Sd_not_found of (Sd.Packed.t * int) [@@deriving sexp]

type packed = P : 'a t -> packed

val dependencies : 'a t -> int Map.M(Sd.Packed).t
val curr_req : 'a t -> Set.M(Sd.Packed).t

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

(* returned function is effectively find *)
val sd_history : 'a Sd.t -> int -> (int -> 'a option) t

(* gets the entire current state *)
val state : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> Rs.t t

(* gets an entire previous state *)
val state_past : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> int -> Rs.t t

(* gets the entire state history *)
val full_rsh : unit -> Rsh.t t

(* Allows you to apply another function to the given state history in a lazy manner *)
val lazy_sd_func : 'a t -> (unit -> 'a) t