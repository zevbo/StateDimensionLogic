open! Core
open Src

module F (Sd : Robot_state.Sd) : sig
  type t =
    < current_sds_required : Set.Make(Sd).t
    ; past_sds_required : Set.Make(Sd).t
    ; sds_estimating : Set.Make(Sd).t
    ; estimate : Robot_state_history.F(Sd).t -> Robot_state.F(Sd).t
    ; uses_measrumeants : bool
    ; get_uncertianty : Sd.t -> Uncertianty.t option >
end