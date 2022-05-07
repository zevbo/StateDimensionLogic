open! Core
open! Sd_logic
open! Sd_func
open! State_estimators

module Filterable_float : sig
  include Particle_filter.Filterable with type t = float
end = struct
  include Float

  let average floats =
    List.sum (module Float) floats ~f:(fun f -> f) /. Float.of_int (List.length floats)
  ;;

  let add_error f error_f = f +. Random.float_range (-1.0 *. error_f) error_f
end

module Fitlerable_Vec : sig
  include Particle_filter.Filterable with type t = Vec.t
end = struct
  include Vec

  let average vecs =
    Vec.scale
      (List.sum (module Vec) vecs ~f:(fun i -> i))
      (1.0 /. Float.of_int (List.length vecs))
  ;;

  let add_error vec error_vec =
    Vec.create
      (Filterable_float.add_error vec.x error_vec.x)
      (Filterable_float.add_error vec.y error_vec.y)
  ;;
end

let sds_estimating_and_info =
  [ Particle_filter.P
      ( (module Fitlerable_Vec : Particle_filter.Filterable with type t = Vec.t)
      , Mupdate_pos.mpos_sd
      , Vec.create (1.0 *. Specs.dt) (1.0 *. Specs.dt) )
  ; Particle_filter.P
      ( (module Filterable_float : Particle_filter.Filterable with type t = float)
      , Mupdate_pos.mangle_sd
      , 0.01 *. Specs.dt )
  ]
;;

let rellocate_margin = 1.
let alpha = 0.9

let gps_judge =
  let+ gps = sd Sensors.gps.gps_pos_sd
  and+ pos = sd Mupdate_pos.mpos_sd in
  let dist = Vec.dist gps pos in
  if Float.(dist > Sensors.gps.rellocate_dist) then 0. else alpha ** dist
;;

let start = Rsh.create ~min_default_length:2 ()

let pf =
  Particle_filter.create_est
    ~start
    ~sds_estimating_and_info
    ~judge:gps_judge
    ~est:Mupdate_pos.est
    ~num_particles:100
;;

let pf_small =
  Particle_filter.create_est
    ~start
    ~sds_estimating_and_info
    ~judge:gps_judge
    ~est:Mupdate_pos.est
    ~num_particles:10
;;

let pfpf =
  Particle_filter.create_est
    ~start
    ~sds_estimating_and_info
    ~judge:gps_judge
    ~est:pf_small
    ~num_particles:10
;;
