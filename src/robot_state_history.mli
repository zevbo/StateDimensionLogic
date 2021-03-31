open! Core
open RobotState

module RobotStateHistory(SD: SD) : sig 
    type t
    val create : max_length:int -> t 
    val get_state : int -> RobotState(SD).t
    val get_current_state : unit -> RobotState(SD).t
    val add_state : RobotState(SD).t -> unit
    val copy : unit -> t
end