open! Core

type t =
  { logic : Robot_state.t Sd_lang.t
  ; sds_estimating : Set.M(Sd.Packed).t
  }

let create logic sds_estimating = { logic; sds_estimating }

type safety =
  | Safe
  | Warnings
  | Unsafe

exception Missing_sd of string
exception Extra_sd of string

let execute ~safety t rsh =
  let estimated_state = Sd_lang.execute t.logic rsh in
  let expected_keys = t.sds_estimating in
  (match safety with
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
