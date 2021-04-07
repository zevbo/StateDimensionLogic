open! Core
open Robot_state
open Robot_state_history

module type State_estimator = sig 
    type t 
    val current_sds_required : SD.t list
    val past_sds_required : SD.t list
    val sds_estimating : SD.t list
    val estimate : Robot_state_history(SD).t -> Robot_state(SD).t
end

val estimate : Robot_state_history.t -> StateEstimator.t -> unit