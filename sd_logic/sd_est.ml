open! Core

module Equal_sd = struct
  module T = struct
    type t =
      | Eq : ('a Sd.t * ('a -> 'a -> bool)) -> t
      | Reg : 'a Sd.t -> t

    let packed_sd t =
      match t with
      | Eq (sd, _) -> Sd.pack sd
      | Reg sd -> Sd.pack sd
    ;;

    let sexp_of_t t = Sd.Packed.sexp_of_t (packed_sd t)
    let compare t1 t2 = Sd.Packed.compare (packed_sd t1) (packed_sd t2)
  end

  include T
  include Comparator.Make (T)
end

module E = Equal_sd

let sd sd_ = E.Reg sd_
let eq_sd sd f = E.Eq (sd, f)

type t =
  { logic : Robot_state.t Sd_func.t
  ; sds_estimating : Set.M(Equal_sd).t
  ; is_prim : bool
  }

let sds_estimating_set t = Set.map (module Sd.Packed) t.sds_estimating ~f:E.packed_sd

let create_set ?(unstable = true) logic sds_estimating =
  { logic; sds_estimating; is_prim = unstable }
;;

let create ?(unstable = true) logic sds_estimatingl =
  { logic
  ; sds_estimating = Set.of_list (module Equal_sd) sds_estimatingl
  ; is_prim = unstable
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