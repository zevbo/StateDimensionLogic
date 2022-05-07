open! Core

type t =
  { logic : Robot_state.t Sd_func.t
  ; sds_estimating : Set.M(Sd.Packed).t
  }

let create_set logic sds_estimating = { logic; sds_estimating }

let create logic sds_estimatingl =
  { logic; sds_estimating = Set.of_list (module Sd.Packed) sds_estimatingl }
;;

type safety_info = { max_safety : Safety_level.t }

type safety =
  { missing_sd : Safety_level.t
  ; extra_sd : Safety_level.t
  ; info : safety_info
  }

let create_safety
    ?(default = Safety_level.Safe)
    ?(missing_sd = default)
    ?(extra_sd = default)
    ()
  =
  let info = { max_safety = Safety_level.max missing_sd extra_sd } in
  { missing_sd; extra_sd; info }
;;

exception Missing_sd of Sd.Packed.t [@@deriving sexp]
exception Extra_sd of Sd.Packed.t [@@deriving sexp]

let execute ~safety t rsh =
  let estimated_state = Sd_func.execute t.logic rsh in
  let expected_keys = t.sds_estimating in
  (match safety.info.max_safety with
  | Unsafe -> ()
  | Safe | Warnings ->
    let missing =
      Set.find ~f:(fun key -> not (Robot_state.memp estimated_state key)) expected_keys
    in
    let extra =
      Set.find
        ~f:(fun key -> not (Set.mem expected_keys key))
        (Robot_state.keys estimated_state)
    in
    let run_check one_safety result exc msg =
      match one_safety, result with
      | Safety_level.Unsafe, _ | _, None -> ()
      | Safety_level.Warnings, Some sd -> printf msg (Sd.Packed.to_string sd)
      | Safety_level.Safe, Some sd -> raise (exc sd)
    in
    run_check
      safety.missing_sd
      missing
      (fun sd -> Missing_sd sd)
      "Sd_node.Applicable warning: Detected missing sd %s during application";
    run_check
      safety.extra_sd
      extra
      (fun sd -> Extra_sd sd)
      "Sd_node.Applicable warning: Detected missing sd %s during application");
  estimated_state
;;