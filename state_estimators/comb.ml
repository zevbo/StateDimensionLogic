open! Core
open! Sd_logic
open Sd_func

exception Unequal_estimation

let combine ~switch (est1 : Sd_est.t) (est2 : Sd_est.t) =
  let diff = Set.diff est1.sds_estimating est2.sds_estimating in
  if Set.length diff > 0 then raise Unequal_estimation;
  let logic =
    (* zTODO: need to make this lazy *)
    let+ (use_first : bool) = switch
    and+ r1 = lazy_sd_func est1.logic
    and+ r2 = lazy_sd_func est2.logic in
    if use_first then r1 () else r2 ()
  in
  Sd_est.create_set logic est1.sds_estimating
;;