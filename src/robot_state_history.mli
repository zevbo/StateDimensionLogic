open! Core

type t

(** [create ~max_length] creates a new [t] which stores at most [~max_length] states *)
val create : max_length:int -> t

(** [get_state t i] returns [Some] of the [i]th most recent state from [t], or [None] if [t] doesn't have more than [i] states *)
val nth_state : t -> int -> Robot_state.t option

(** [get_current_state t] is equivilant to [get_state 0]*)
val curr_state : t -> Robot_state.t

(** [add_state t state] adds [state] to [t] *)
val add_state : t -> t

(** [use t state] replaces the current state with [Robot_state.use (curr_state t) state] *)
val use : t -> Robot_state.t -> t

val find : t -> 'a Sd.t -> 'a option
val find_past : t -> int -> 'a Sd.t -> 'a option
val mem : t -> 'a Sd.t -> bool
val mem_past : t -> int -> 'a Sd.t -> bool option
val memp : t -> Sd.Packed.t -> bool
val memp_past : t -> int -> Sd.Packed.t -> bool option