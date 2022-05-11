open! Core

type safety_info = { run_checks : bool }

type safety =
  { premature_sd_req : Safety_level.t
  ; overwritten_sd : Safety_level.t
  ; never_written_sd_req : Safety_level.t
  ; node_safety : Sd_est.safety
  ; info : safety_info
  }

let create_safety
    ?(default = Safety_level.Safe)
    ?(premature_sd_req = default)
    ?(overwritten_sd = default)
    ?(never_written_sd_req = default)
    ?(node_safety = Sd_est.create_safety ~default ())
    ()
  =
  let info =
    { run_checks =
        not
          (Safety_level.compare
             (Safety_level.max
                (Safety_level.max premature_sd_req overwritten_sd)
                never_written_sd_req)
             Safety_level.Unsafe
          = 0)
    }
  in
  { premature_sd_req; overwritten_sd; never_written_sd_req; node_safety; info }
;;

type t =
  { nodes : Sd_est.t list
  ; last_change : int Map.M(Sd.Packed).t
  ; safety : safety
  ; rsh : Rsh.t
  ; end_cond : bool Sd_func.t option
  }

let rsh t = t.rsh

(* only keys with n = 1 *)
let apply t =
  let last_change, rsh =
    List.fold_left
      t.nodes
      ~init:(t.last_change, t.rsh)
      ~f:(fun (last_change, state_history) node ->
        let rerun =
          node.is_prim
          || Rsh.length state_history = 1
          || not
               (Map.for_alli (Sd_func.dependencies node.logic) ~f:(fun ~key ~data ->
                    data < Map.find_exn last_change key))
        in
        let est_set = Sd_est.sds_estimating_set node in
        let estimated_state, changes =
          if rerun
          then (
            let est_state =
              Sd_est.execute ~safety:t.safety.node_safety node state_history
            in
            let changes =
              Set.filter_map
                (module Sd.Packed)
                node.sds_estimating
                ~f:(fun sds_estimating ->
                  match sds_estimating with
                  | Reg sd -> Some (Sd.pack sd)
                  | Eq (sd, equal) ->
                    (match Rsh.find_past state_history 1 sd with
                    | None -> Some (Sd.pack sd)
                    | Some v ->
                      if equal (Rs.find_exn est_state sd) v
                      then None
                      else Some (Sd.pack sd)))
            in
            est_state, changes)
          else
            ( Rs.trim_to (Option.value_exn (Rsh.nth_state state_history 1)) est_set
            , Set.empty (module Sd.Packed) )
        in
        let last_change =
          Set.fold est_set ~init:last_change ~f:(fun lc sd ->
              Map.set
                lc
                ~key:sd
                ~data:
                  (if Set.mem changes sd
                  then 0
                  else 1 + Option.value (Map.find lc sd) ~default:0))
        in
        last_change, Robot_state_history.use state_history estimated_state)
  in
  { t with rsh; last_change }
;;

exception Premature_sd_req of Sd.Packed.t [@@deriving sexp]
exception Overwriting_sd_estimate of Sd.Packed.t [@@deriving sexp]
exception Never_written_req of Sd.Packed.t [@@deriving sexp]

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
      let required, estimating =
        Sd_func.curr_req node.logic, Sd_est.sds_estimating_set node
      in
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
      ~f:(fun full_estimating node ->
        Set.union full_estimating (Sd_est.sds_estimating_set node))
  in
  let non_guranteed set = Set.find set ~f:(fun sd -> not (Set.mem full_estimating sd)) in
  let all_deps = List.map t.nodes ~f:(fun node -> Sd_func.dependencies node.logic) in
  let all_deps =
    match t.end_cond with
    | None -> all_deps
    | Some end_cond -> Sd_func.dependencies end_cond :: all_deps
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

let sd_lengths (nodes : Sd_est.t list) =
  let max_indecies =
    List.fold
      nodes
      ~init:(Map.empty (module Sd.Packed))
      ~f:(fun sd_lengths node ->
        Map.merge_skewed
          sd_lengths
          (Sd_func.dependencies node.logic)
          ~combine:(fun ~key:_key v1 v2 -> 1 + Int.max v1 v2))
  in
  Map.map max_indecies ~f:(fun n -> n + 1)
;;

let create ?(end_cond : bool Sd_func.t Option.t) nodes =
  let sd_lengths = sd_lengths nodes in
  let safety = create_safety () in
  let model =
    { safety
    ; last_change = Map.empty (module Sd.Packed)
    ; nodes
    ; rsh = Rsh.create ~sd_lengths ()
    ; end_cond
    }
  in
  let exec_failure safety_level exc warning sd =
    match safety_level with
    | Safety_level.Unsafe -> ()
    | Safety_level.Warnings ->
      printf
        "Sd_est.Applicable warning: Detected %s of sd %s\n"
        warning
        (Sd.Packed.to_string sd)
    | Safety_level.Safe -> raise exc
  in
  if model.safety.info.run_checks
  then (
    match check model with
    | Passed -> ()
    | Failure (Premature, sd) ->
      exec_failure safety.premature_sd_req (Premature_sd_req sd) "premature require" sd
    | Failure (Overwrite, sd) ->
      exec_failure safety.overwritten_sd (Overwriting_sd_estimate sd) "overwrite" sd
    | Failure (Never_written, sd) ->
      exec_failure
        safety.never_written_sd_req
        (Never_written_req sd)
        "unestimated past require"
        sd);
  model
;;

let tick t =
  let t = apply t in
  { t with rsh = Rsh.add_empty_state t.rsh }
;;

let rec run_checked ?(no_end_cond = false) ?(min_ms = 0.0) ?(max_ticks = 0) t =
  let desired_time = Unix.time () +. (min_ms /. 1000.0) in
  let t = apply t in
  let to_end =
    (not no_end_cond)
    &&
    match t.end_cond with
    | None -> false
    | Some end_cond -> Sd_func.execute end_cond t.rsh
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