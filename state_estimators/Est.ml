open! Core
open Src

(* TODO: decrease duplication *)

module type W_state = sig
  type t

  (* todo: make these immutable *)
  val current_sds_required : Sd.Packed.t Hash_set.t
  val past_sds_required : Sd.Packed.t Hash_set.t
  val sds_estimating : Sd.Packed.t Hash_set.t
  val est : t -> Robot_state_history.t -> Robot_state.t

  (* TODO: should allow more types of uncertainty, as well as a function that gives covariance *)
  val uncertainty : t -> Robot_state_history.t -> float Sd.t -> Uncertianty.t option
end

module type Test = sig end

module type Wo_state = sig
  include W_state with type t = unit

  val est_stateless : Robot_state_history.t -> Robot_state.t
  val uncertainty_stateless : Robot_state_history.t -> float Sd.t -> Uncertianty.t option
end

module Applicable = struct
  type t = P : (module W_state with type t = 'a) * 'a -> t

  type safety =
    | Safe
    | Warnings
    | Unsafe

  type model =
    { ts : t list
    ; safety : safety
    }

  let create (type a) (module W_state : W_state with type t = a) (w_t : a) =
    P ((module W_state), w_t)
  ;;

  exception Missing_sd of Sd.Packed.t
  exception Extra_sd of Sd.Packed.t

  let packed_to_str packed = String.t_of_sexp (Sd.Packed.sexp_of_t packed)

  let apply (state_history : Robot_state_history.t) (model : model) =
    List.fold_left model.ts ~init:state_history ~f:(fun state_history t ->
        match t with
        | P ((module W_state), t) ->
          let estimated_state = W_state.est t state_history in
          (match model.safety with
          | Unsafe -> ()
          | Safe | Warnings ->
            let missing =
              Hash_set.find
                ~f:(fun key -> not (Robot_state.memp estimated_state key))
                W_state.sds_estimating
            in
            let extra =
              List.find
                ~f:(fun key -> not (Hash_set.mem W_state.sds_estimating key))
                (Robot_state.keys estimated_state)
            in
            (match model.safety, missing, extra with
            | Unsafe, _, _ -> (* should never reach here *) ()
            | Safe, Some sd, _ -> raise (Missing_sd sd)
            | Safe, None, Some sd -> raise (Extra_sd sd)
            | Warnings, Some sd, _ ->
              printf
                "Est.Applicable warning: Detected missing sd %s during application"
                (packed_to_str sd)
            | Warnings, None, Some sd ->
              printf
                "Est.Applicable warning: Detected extra sd %s during application"
                (packed_to_str sd)
            | _, None, None -> ()));
          Robot_state_history.use
            state_history
            ~to_use:(Some (Hash_set.to_list W_state.sds_estimating))
            estimated_state)
  ;;

  exception Premature_sd_req of Sd.Packed.t
  exception Overwriting_sd_estimate of Sd.Packed.t
  exception Never_written_req of Sd.Packed.t

  type check_failure =
    | Premature
    | Overwrite
    | Never_written

  type check_status =
    | Passed
    | Failure of check_failure * Sd.Packed.t

  let hash_set_mutable_union h1 h2 = Hash_set.iter h2 ~f:(fun key -> Hash_set.add h1 key)

  let current_check (model : model) =
    List.fold_until
      ~init:(Hash_set.create (module Sd.Packed))
      ~f:(fun guaranteed t ->
        let required, estimating =
          match t with
          | P ((module W_state), _t) ->
            W_state.current_sds_required, W_state.sds_estimating
        in
        let premature_sd =
          List.find (Hash_set.to_list required) ~f:(fun sd ->
              not (Hash_set.mem guaranteed sd))
        in
        let overwritten_sd =
          List.find (Hash_set.to_list estimating) ~f:(Hash_set.mem guaranteed)
        in
        match premature_sd, overwritten_sd with
        | Some premature_sd, _ ->
          print_endline (String.t_of_sexp (Sd.Packed.sexp_of_t premature_sd));
          Continue_or_stop.Stop (Failure (Premature, premature_sd))
        | None, Some overwritten_sd ->
          Continue_or_stop.Stop (Failure (Overwrite, overwritten_sd))
        | None, None ->
          hash_set_mutable_union guaranteed estimating;
          Continue_or_stop.Continue guaranteed)
      ~finish:(fun _ -> Passed)
      model.ts
  ;;

  let past_check (model : model) =
    let full_estimating = Hash_set.create (module Sd.Packed) in
    List.iter model.ts ~f:(fun t ->
        match t with
        | P ((module W_state), _est) ->
          hash_set_mutable_union full_estimating W_state.sds_estimating);
    let non_guranteed set =
      List.filter (Hash_set.to_list set) ~f:(fun sd ->
          not (Hash_set.mem full_estimating sd))
    in
    let all_non_guaranteed =
      List.fold
        (List.map model.ts ~f:(fun t ->
             match t with
             | P ((module W_state), _est) -> non_guranteed W_state.past_sds_required))
        ~init:[]
        ~f:List.append
    in
    match all_non_guaranteed with
    | [] -> Passed
    | hd :: _tl -> Failure (Never_written, hd)
  ;;

  let check (model : model) =
    let current_check = current_check model in
    (* todo: add past check as well *)
    match current_check with
    | Passed -> past_check model
    | status -> status
  ;;

  let create_model ?(safety = Safe) ts =
    let model = { safety; ts } in
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
        printf "Est.Applicable warning: Detected %s of sd %s\n" warning (packed_to_str sd);
        model)
  ;;
end