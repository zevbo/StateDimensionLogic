open! Core
open Robot_state

module Robot_state_history(SD: SD) : sig 
    type t
    
    (** [create ~max_length] creates a new [t] which stores at most [~max_length] states *)
    val create : max_length:int -> t 

    (** [get_state t i] returns [Some] of the [i]th most recent state from [t], or [None] if [t] doesn't have more than [i] states *)
    val get_state : t -> int -> Robot_state(SD).t option

    (** [get_current_state t] is equivilant to [get_state 0]*)
    val get_current_state : t -> Robot_state(SD).t option

    (** [add_state t state] adds [state] to [t] *)
    val add_state : t -> Robot_state(SD).t -> unit

    (** [copy t] creates a deep copy of [t] *)
    val copy : t -> t
end