open! Core

type t =
  { logic : Robot_state.t Sd_lang.t
  ; sds_estimating : Set.M(Sd.Packed).t
  }

let create logic sds_estimating = { logic; sds_estimating }

type one_safety =
  | Unsafe
  | Warnings
  | Safe
[@@deriving compare]

let max_safety s1 s2 = if compare_one_safety s1 s2 > 0 then s1 else s2

type safety_info = { max_safety : one_safety }

type safety =
  { missing_sd : one_safety
  ; extra_sd : one_safety
  ; info : safety_info
  }

let create_safety ?(default = Safe) ?(missing_sd = default) ?(extra_sd = default) =
  let info = { max_safety = max_safety missing_sd extra_sd } in
  { missing_sd; extra_sd; info }
;;

exception Missing_sd of string
exception Extra_sd of string

let execute ~safety t rsh =
  let estimated_state = Sd_lang.execute t.logic rsh in
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
      | Unsafe, _ | _, None -> ()
      | Warnings, Some sd -> printf msg (Sd.Packed.to_string sd)
      | Safe, Some sd -> raise (exc (Sd.))
    in
    (match safety, missing, extra with
    | Unsafe, _, _ -> (* should never reach here *) ()
    | Safe, Some sd, _ -> raise (Missing_sd (Sd.Packed.to_string sd))
    | Safe, None, Some sd -> raise (Extra_sd (Sd.Packed.to_string sd))
    | Warnings, Some sd, _ ->
      printf
        "Sd_node.Applicable warning: Detected missing sd %s during application"
        (Sd.Packed.to_string sd)
    | Warnings, None, Some sd ->
      printf
        "Sd_node.Applicable warning: Detected extra sd %s during application"
        (Sd.Packed.to_string sd)
    | _, None, None -> ()));
  estimated_state
;;
