open! Core
open Robot_state

module Robot_state_history(SD: SD) : sig 
    type t
    val create : max_length:int -> t 
    val get_state : t -> int -> Robot_state(SD).t option
    val get_current_state : t -> Robot_state(SD).t option
    val add_state : t -> Robot_state(SD).t -> unit
    val copy : t -> t
end