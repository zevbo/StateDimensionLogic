open! Core
open! Sd_logic

exception Unequal_estimation

let combine ~switch (est1 : Sd_node.t) (est2 : Sd_node.t) =
  let diff = Set.diff est1.sds_estimating est2.sds_estimating in
  if Set.length diff > 0 then raise Unequal_estimation;
  let logic =
    (* need to make this lazy *)
    [%map_open.Sd_lang
      let (use_first : bool) = switch
      and r1 = est1.logic
      and r2 = est2.logic in
      if use_first then r1 else r2]
  in
  Sd_node.create logic est1.sds_estimating
;;