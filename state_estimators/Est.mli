open! Core
open Src

module F (SD : Robot_state.SD) : sig
  type t =
    < current_sds_required : Set.Make(SD).t
    ; past_sds_required : Set.Make(SD).t
    ; sds_estimating : Set.Make(SD).t
    ; estimate : Robot_state_history.F(SD).t -> Robot_state.F(SD).t
    ; uses_measrumeants : bool
    ; get_uncertianty : SD.t -> Uncertianty.t option >
end