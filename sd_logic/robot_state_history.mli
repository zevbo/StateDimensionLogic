open! Core

type t [@@deriving sexp_of]

(**
Creates a [t]. 

[?sd_lengths] Specify amount of history to keep for particular state dimension.

[?min_default_length] defaults to 1. Amount of history to keep for any state dimension 
not mentioned in [sd_lengths] is the maximum of [min_default_lengths] and the largest length in [~sd_lengths]

[?forced_default_length] allows you to use a default length that is smaller than some in [~sd_lengths], at the cost of runtime. 
The time complexity for [use] with a state that has m bindings to a t with n states and k entries in sd_lengths
goes from [O(log(n))] to [O(log(n) + mlog(k))]. Also, the size of sd_lengths will have an entry for each
Sd.t that has a binding in an Rs.t that is added to the t at anytime.
*)
val create
  :  ?sd_lengths:(Sd.Packed.t, int, Sd.Packed.comparator_witness) Map.t
  -> ?min_default_length:int
  -> ?forced_default_length:int
  -> unit
  -> t

(** [nth_state t i] returns [Some] of the [i]th most recent state from
   [t], or [None] if [t] doesn't have more than [i] states. O(log(n))
   time complexity in length of [t].  *)
val nth_state : t -> int -> Robot_state.t option

(** [get_current_state t] is equivalent to [get_state 0]. O(1) time
   complexity. *)
val curr_state : t -> Robot_state.t

(** [add_state t state] adds [state] to [t]. O(log(n)) time complexity
   in length of [t]. *)
val add_empty_state : t -> t

val add_state : t -> Robot_state.t -> t

(** [use t state] replaces the current state with [Robot_state.use
   (curr_state t) state] *)
val use : t -> ?to_use:Sd.set option -> Robot_state.t -> t

val use_extras : t -> Robot_state.t -> t

(** [find t sd] is equivilant to [Robot_state.find (curr_state t) sd] *)
val find : t -> 'a Sd.t -> 'a option

val find_exn : t -> 'a Sd.t -> 'a

(** [find_past t n sd] is equivalent to [Robot_state.find (Option.value_exn (nth_state t
   n)) sd] when there are at least [n + 1] states. *)
val find_past : t -> int -> 'a Sd.t -> 'a option

val find_past_exn : t -> int -> 'a Sd.t -> 'a
val find_past_def : t -> default:'a -> int -> 'a Sd.t -> 'a
val find_past_last_def : t -> int -> 'a Sd.t -> 'a option

(** [mem t sd] is equivilant to [Robot_state.mem (curr_state t) sd] *)
val mem : t -> 'a Sd.t -> bool

(** [mem_past t n sd] is equivilant to [Robot_state.mem (nth_state t
   n) sd] *)
val mem_past : t -> int -> 'a Sd.t -> bool option

(** [memp t sd] is equivilant to [Robot_state.memp (curr_state t) sd] *)
val memp : t -> Sd.Packed.t -> bool

(** [memp_past t n sd] is equivilant to [Robot_state.memp (nth_state t
   n) sd] *)
val memp_past : t -> int -> Sd.Packed.t -> bool option

(** [default_length t] returns the maximum length of [t]. O(1) time
   complexity. *)
val default_length : t -> int

(** [length t] returns the length of [t]. O(n) time complexity in the
   length of [t]. *)
val length : t -> int
