open! Core

type safety =
  | Safe
  | Warnings
  | Unsafe

type t =
  { nodes : Sd_node.t list
  ; safety : safety
  ; rsh : Rsh.t
  ; end_cond : bool Sd_lang.t option
  }

let key_dependencies logic =
  let dep = Sd_lang.dependencies logic in
  let curr_dep = Map.filter dep ~f:(fun n -> n = 0) in
  Map.key_set curr_dep
;;

let apply t =
  List.fold_left t.nodes ~init:t.rsh ~f:(fun state_history node ->
      let est_safety =
        match t.safety with
        | Unsafe -> Sd_node.Unsafe
        | Warnings -> Sd_node.Warnings
        | Safe -> Sd_node.Safe
      in
      let estimated_state = Sd_node.execute ~safety:est_safety node state_history in
      Robot_state_history.use state_history estimated_state)
;;

exception Premature_sd_req of Sd.Packed.t [@@derving sexp]
exception Overwriting_sd_estimate of Sd.Packed.t [@@derving sexp]
exception Never_written_req of Sd.Packed.t [@@derving sexp]

type check_failure =
  | Premature
  | Overwrite
  | Never_written

type check_status =
  | Passed
  | Failure of check_failure * Sd.Packed.t

let current_check (t : t) =
  List.fold_until
    ~init:(Set.empty (module Sd.Packed))
    ~f:(fun guaranteed node ->
      let required, estimating = key_dependencies node.logic, node.sds_estimating in
      let premature_sd = Set.find required ~f:(fun sd -> not (Set.mem guaranteed sd)) in
      let overwritten_sd = Set.find estimating ~f:(Set.mem guaranteed) in
      match premature_sd, overwritten_sd with
      | Some premature_sd, _ -> Continue_or_stop.Stop (Failure (Premature, premature_sd))
      | None, Some overwritten_sd ->
        Continue_or_stop.Stop (Failure (Overwrite, overwritten_sd))
      | None, None -> Continue_or_stop.Continue (Set.union guaranteed estimating))
    ~finish:(fun _ -> Passed)
    t.nodes
;;

let past_check t =
  let full_estimating =
    List.fold_left
      t.nodes
      ~init:(Set.empty (module Sd.Packed)) (* zTODO: fix to better Set.union *)
      ~f:(fun full_estimating node -> Set.union full_estimating node.sds_estimating)
  in
  let non_guranteed set = Set.find set ~f:(fun sd -> not (Set.mem full_estimating sd)) in
  let all_deps = List.map t.nodes ~f:(fun node -> Sd_lang.dependencies node.logic) in
  let all_deps =
    match t.end_cond with
    | None -> all_deps
    | Some end_cond -> Sd_lang.dependencies end_cond :: all_deps
  in
  match List.find_map all_deps ~f:(fun deps -> non_guranteed (Map.key_set deps)) with
  | None -> Passed
  | Some sd -> Failure (Never_written, sd)
;;

let check t =
  (* todo: add past check as well *)
  match past_check t with
  | Passed -> current_check t
  | status -> status
;;

let sd_lengths (nodes : Sd_node.t list) =
  let max_indecies =
    List.fold
      nodes
      ~init:(Map.empty (module Sd.Packed))
      ~f:(fun sd_lengths node ->
        Map.merge_skewed
          sd_lengths
          (Sd_lang.dependencies node.logic)
          ~combine:(fun ~key:_key -> Int.max))
  in
  Map.map max_indecies ~f:(fun n -> n + 1)
;;

let create ?(safety = Safe) ?(end_cond : bool Sd_lang.t Option.t) nodes =
  let sd_lengths = sd_lengths nodes in
  let model = { safety; nodes; rsh = Rsh.create ~sd_lengths (); end_cond } in
  match safety with
  | Unsafe -> model
  | Safe ->
    (match check model with
    | Passed -> model
    | Failure (Premature, sd) -> raise (Premature_sd_req sd)
    | Failure (Overwrite, sd) -> raise (Overwriting_sd_estimate sd)
    | Failure (Never_written, sd) -> raise (Never_written_req sd))
  | Warnings ->
    (match check model with
    | Passed -> model
    | Failure (error, sd) ->
      let warning =
        match error with
        | Premature -> "premature require"
        | Overwrite -> "possible overwrite"
        | Never_written -> "unestimated past require"
      in
      printf
        "Sd_node.Applicable warning: Detected %s of sd %s\n"
        warning
        (Sd.Packed.to_string sd);
      model)
;;

let tick t = { t with rsh = Rsh.add_empty_state (apply t) }

let rec run_checked ?(no_end_cond = false) ?(min_ms = 0.0) ?(max_ticks = 0) t =
  let desired_time = Unix.time () +. (min_ms /. 1000.0) in
  let t = { t with rsh = apply t } in
  let to_end =
    (not no_end_cond)
    &&
    match t.end_cond with
    | None -> false
    | Some end_cond -> Sd_lang.execute end_cond t.rsh
  in
  if to_end || max_ticks = 1
  then ()
  else (
    let t = { t with rsh = Rsh.add_empty_state t.rsh } in
    let max_ticks = max (-1) (max_ticks - 1) in
    let delay = Float.max (desired_time -. Unix.time ()) 0.0 in
    Thread.delay delay;
    run_checked t ~no_end_cond ~min_ms ~max_ticks)
;;

let run ?no_end_cond ?min_ms ?max_ticks t =
  (match max_ticks with
  | None -> ()
  | Some max_ticks -> assert (max_ticks = -1 || max_ticks > 0));
  run_checked ?no_end_cond ?min_ms ?max_ticks t
;;