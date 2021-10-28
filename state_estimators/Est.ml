open! Core
open Src

(* TODO: decrease duplication *)

module type W_state = sig
  type t

  val current_sds_required : Sd.Packed.t Hash_set.t
  val past_sds_required : Sd.Packed.t Hash_set.t
  val sds_estimating : Sd.Packed.t Hash_set.t
  val est : t -> Robot_state_history.t -> Robot_state.t

  (* TODO: should allow more types of uncertainty, as well as a function that gives covariance *)
  val uncertainty : t -> Robot_state_history.t -> float Sd.t -> Uncertianty.t option
end

module type Wo_state = sig
  type t = unit

  val current_sds_required : Sd.Packed.t Hash_set.t
  val past_sds_required : Sd.Packed.t Hash_set.t
  val sds_estimating : Sd.Packed.t Hash_set.t
  val est : t -> Robot_state_history.t -> Robot_state.t
  val est_stateless : Robot_state_history.t -> Robot_state.t

  (* TODO: should allow more types of uncertainty, as well as a function that gives covariance *)
  val uncertainty : t -> Robot_state_history.t -> float Sd.t -> Uncertianty.t option
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

  type check_status =
    | Passed
    | Premature of Sd.Packed.t
    | Overwrite of Sd.Packed.t

  let check (model : model) =
    let finish _ = Passed in
    let current_check =
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
            Continue_or_stop.Stop (Premature premature_sd)
          | None, Some overwritten_sd -> Continue_or_stop.Stop (Overwrite overwritten_sd)
          | None, None -> Continue_or_stop.Continue (Hash_set.union guaranteed estimating))
        ~finish
        model.ts
    in
    (* todo: add past check as well *)
    current_check
  ;;

  let create_model ?(safety = Safe) ts =
    let model = { safety; ts } in
    match safety with
    | Unsafe -> model
    | Safe ->
      (match check model with
      | Passed -> model
      | Premature sd -> raise (Premature_sd_req sd)
      | Overwrite sd -> raise (Overwriting_sd_estimate sd))
    | Warnings ->
      (match check model with
      | Passed -> model
      | Premature sd ->
        printf
          "Est.Applicable warning: Detected premature require of sd %s\n"
          (packed_to_str sd);
        model
      | Overwrite sd ->
        printf
          "Est.Applicable warning: Detected possible overwritte of sd %s\n"
          (packed_to_str sd);
        model)
  ;;
end