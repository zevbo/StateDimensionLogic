open! Core
open Src
open Robot_state
open Robot_state_history

module type Est = functor (SD : SD) -> sig
  type t

  val current_sds_required : SD.t list

  val past_sds_required : SD.t list

  val sds_estimating : SD.t list

  val estimate : t -> Robot_state_history(SD).t -> Robot_state(SD).t

  val uses_measrumeants : bool

  val get_uncertianty : t -> SD.t -> Uncertianty.t option
end

let a = List.sum
