open! Core
open! Sd_logic
open Sd_func

type particle = Rsh.t

type weighted =
  { particle : particle
  ; weight : float
  }

exception Unestimatable_sd of Sd.Packed.t [@@deriving sexp]

module type Filterable = sig
  type t

  val average : t list -> t
  val add_error : t -> t -> t
end

type est_and_info =
  | P : (module Filterable with type t = 'a) * 'a Sd.t * 'a -> est_and_info

let global_id = ref (-1)

let get_id () =
  global_id := !global_id + 1;
  !global_id
;;

(* todo: automatically create start rsh *)
let create_est
    ~start
    ~(est : Sd_est.t)
    ~(judge : float Sd_func.t)
    ~(sds_estimating_and_info : est_and_info List.t)
    ~num_particles
  =
  assert (num_particles > 0);
  (* we can only estimate summable SD. Currently only allowing floats but could allow any summable *)
  let sds_estimating =
    List.map sds_estimating_and_info ~f:(fun (P (_, sd, _)) -> Sd.pack sd)
  in
  let unestimated_sd =
    List.find sds_estimating ~f:(fun sd -> not (Set.mem est.sds_estimating sd))
  in
  (match unestimated_sd with
  | None -> ()
  | Some sd -> raise (Unestimatable_sd sd));
  (* storing the list of particles in this sd *)
  let id = get_id () in
  let particles_sd =
    Sd.create (Printf.sprintf "particles_sd%i" id) (fun (_particles : weighted list) ->
        String.sexp_of_t "particles_sd has no meaninful sexp_of")
  in
  let judge_dep = Map.key_set (Sd_func.dependencies judge) in
  let est_dep =
    Set.filter
      (Map.key_set (Sd_func.dependencies est.logic))
      ~f:(fun sd -> not (Set.mem est.sds_estimating sd))
  in
  (* TODO: check if estimator requires a current value for something it estiamtes *)
  let logic =
    let+ weighted_particles =
      sd_past particles_sd 1 (V [ { weight = 1.0; particle = start } ])
    (* values we need to input to the stimator *)
    and+ inputs = state est_dep
    (* extra values that we need for the judge *)
    and+ extra_judge_vals = state judge_dep in
    (* particles updates to have all the values for the judge *)
    let real_num_particles = List.length weighted_particles in
    List.iter weighted_particles ~f:(fun weighted ->
        assert (Float.(weighted.weight <= 1.0)));
    let total_weight =
      List.sum (module Float) weighted_particles ~f:(fun weighted -> weighted.weight)
    in
    let probability weighted =
      if Float.(total_weight = 0.0)
      then 1.0 /. Float.of_int real_num_particles
      else weighted /. total_weight
    in
    (* note: worst big O is the n log n sort here. Maybe fixable? *)
    (* selection_ns determine the random choice of the particles *)
    let selection_ns =
      List.sort
        (List.init num_particles ~f:(fun _ -> Random.float 1.0))
        ~compare:Float.compare
    in
    let total_selection_weight = ref 0.0 in
    (* randomly select a list of num_particles particles *)
    let rec select_particles ?(on = 0.0) weighted_particles selection_ns =
      match selection_ns, weighted_particles with
      | [], _ -> []
      | _, [] -> failwith "Internal Error on Select Particles"
      | n :: ns, weighted :: particles ->
        let new_on = on +. probability weighted.weight in
        if List.is_empty particles || Float.(n <= new_on)
        then (
          total_selection_weight := !total_selection_weight +. weighted.weight;
          weighted.particle :: select_particles ~on weighted_particles ns)
        else select_particles ~on:new_on particles selection_ns
    in
    let selected_particles = select_particles weighted_particles selection_ns in
    let new_particles =
      List.map selected_particles ~f:(fun particle ->
          (* add required values for estimator *)
          let particle = Rsh.add_state particle inputs in
          let result = Sd_est.execute ~safety:(Sd_est.create_safety ()) est particle in
          let result_w_error =
            List.fold_left
              sds_estimating_and_info
              ~init:result
              ~f:(fun result_w_error (P ((module F), sd, error)) ->
                Rs.set result_w_error sd (F.add_error (Rs.find_exn result sd) error))
          in
          let particle = Rsh.use particle result_w_error in
          particle)
    in
    let extras_added_particles =
      List.map new_particles ~f:(fun particle -> Rsh.use particle extra_judge_vals)
    in
    let weighted_particles =
      List.map extras_added_particles ~f:(fun particle ->
          { particle; weight = Float.max (Sd_func.execute judge particle) 0.0 })
    in
    (* determine average value to find particle we want *)
    let avg_value (type a) (module F : Filterable with type t = a) (sd : a Sd.t) =
      F.average (List.map new_particles ~f:(fun particle -> Rsh.find_exn particle sd))
    in
    let rs =
      List.fold sds_estimating_and_info ~init:Rs.empty ~f:(fun rs (P (m, sd, _)) ->
          Rs.set rs sd (avg_value m sd))
    in
    Rs.set rs particles_sd weighted_particles
  in
  Sd_est.create logic (Sd.pack particles_sd :: sds_estimating)
;;