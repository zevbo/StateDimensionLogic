open! Core
open! Sd_logic

type particle = Rsh.t

type weighted =
  { particle : particle
  ; weight : float
  }

exception Unestimatable_sd of string

let create_logic
    start
    (node : Sd_est.t)
    (judge : float Sd_lang.t)
    (sds_estimating : float Sd.t List.t)
    num_particles
  =
  assert (num_particles > 0);
  (* we can only estimate summable SD. Currently only allowing floats but could allow any summable *)
  let unestimated_sd =
    List.find sds_estimating ~f:(fun sd -> not (Set.mem node.sds_estimating (Sd.pack sd)))
  in
  (match unestimated_sd with
  | None -> ()
  | Some sd -> raise (Unestimatable_sd (Sd.Packed.to_string (Sd.pack sd))));
  (* storing the list of particles in this sd *)
  let particles_sd =
    Sd.create "particles_sd" (fun (_particles : particle list) ->
        String.sexp_of_t "particles_sd has no meaninful sexp_of")
  in
  [%map_open.Sd_lang
    let particles = sd_past particles_sd 1 (V start)
    (* values we need to input to the stimator *)
    and inputs = state (Map.key_set (Sd_lang.dependencies node.logic))
    (* extra values that we need for the judge *)
    and extra_judge_vals = state_past (Map.key_set (Sd_lang.dependencies judge)) 1 in
    (* particles updates to have all the values for the judge *)
    let extras_added_particles =
      List.map particles ~f:(fun particle -> Rsh.use particle extra_judge_vals)
    in
    let weighted_particles =
      List.map extras_added_particles ~f:(fun particle ->
          { particle; weight = Float.max (Sd_lang.execute judge particle) 0.0 })
    in
    let total_weight =
      List.sum (module Float) weighted_particles ~f:(fun weighted -> weighted.weight)
    in
    let real_num_particles = List.length particles in
    let probability weighted =
      if Float.(total_weight = 0.0)
      then 1.0 /. Float.of_int real_num_particles
      else weighted /. total_weight
    in
    (* note: worst big O is the n log n sort here. Maybe fixable? *)
    (* selection_ns determine the random choice of the particles *)
    let selections_ns =
      List.sort
        (List.init num_particles ~f:(fun _ -> Random.float 1.0))
        ~compare:Float.compare
    in
    (* randomly select a list of num_particles particles *)
    let rec select_particles ?(on = 0.0) weighted_particles selection_ns =
      match selection_ns, weighted_particles with
      | [], _ -> []
      | _, [] -> failwith "Internal Error on Select Particles"
      | n :: ns, weighted :: particles ->
        let on = on +. probability n in
        if List.is_empty particles || Float.(n <= on)
        then weighted.particle :: select_particles ~on particles ns
        else select_particles ~on particles selections_ns
    in
    let selected_particles = select_particles weighted_particles selections_ns in
    let new_particles =
      List.map selected_particles ~f:(fun particle ->
          (* add required values for estimator *)
          let particle = Rsh.add_state particle inputs in
          let particle =
            Rsh.use particle (Sd_est.execute ~safety:Sd_est.Safe node particle)
          in
          particle)
    in
    (* determine average value to find particle we want *)
    let total_value sd =
      List.sum (module Float) new_particles ~f:(fun particle -> Rsh.find_exn particle sd)
    in
    let avg_value sd = total_value sd /. Float.of_int num_particles in
    let rs =
      List.fold sds_estimating ~init:Rs.empty ~f:(fun rs sd ->
          Rs.set rs sd (avg_value sd))
    in
    Rs.set rs particles_sd new_particles]
;;
