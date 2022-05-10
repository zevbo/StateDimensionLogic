open! Core

module Equal_sd = struct
  module T = struct
    type t = E : ('a Sd.t * ('a -> 'a -> bool)) -> t

    let sexp_of_t (E (a, _)) = Sd.Packed.sexp_of_t (P a)
    let compare (E (a, _)) (E (b, _)) = Sd.Packed.compare (P a) (P b)
    let create sd eq = E (sd, eq)
  end

  include T
  include Comparator.Make (T)
end

type sds_estimating =
  | Reg of Set.M(Sd.Packed).t
  | Reactive of Set.M(Equal_sd).t

type t =
  { logic : Robot_state.t Sd_func.t
  ; sds_estimating : sds_estimating
  ; signal : bool
  }

let sds_estimating_set t =
  match t.sds_estimating with
  | Reg set -> set
  | Reactive equal_set -> Set.map (module Sd.Packed) equal_set ~f:(fun (E (a, _)) -> P a)
;;

let create_set ?(signal = true) logic sds_estimating =
  { logic; sds_estimating = Reg sds_estimating; signal }
;;

let create ?(signal = true) logic sds_estimatingl =
  { logic; sds_estimating = Reg (Set.of_list (module Sd.Packed) sds_estimatingl); signal }
;;

let create_reactive logic equal_sds_estimating ~signal =
  { logic
  ; sds_estimating = Reactive (Set.of_list (module Equal_sd) equal_sds_estimating)
  ; signal
  }
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
  let expected_keys = sds_estimating_set t in
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