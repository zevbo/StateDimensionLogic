open! Sd_logic
open! Sd_func
open! Core

type t =
  { error_per : float
  ; rellocate_dist : float
  ; rellocate_error : float
  ; pos_sd : Vec.t Sd.t
  ; gps_pos_sd : Vec.t Sd.t
  }

let create pos_sd error_per rellocate_dist rellocate_error =
  let gps_pos_sd = Sd.create "gps_pos" Vec.sexp_of_t in
  { error_per; rellocate_dist; rellocate_error; pos_sd; gps_pos_sd }
;;

let random_vec error =
  let error_mag = Random.float_range 0.0 error in
  let error_angle = Random.float_range Vec.min_angle Vec.max_angle in
  Vec.rotate (Vec.create error_mag 0.0) error_angle
;;

let rellocate_to (t : t) real_pos = Vec.add real_pos (random_vec t.rellocate_error)

let update_logic t =
  let+ prev_gps_pos = sd_past t.gps_pos_sd 1 Op
  and+ prev_pos = sd_past t.pos_sd 1 Op
  and+ pos = sd t.pos_sd in
  let gps_pos =
    match prev_pos, prev_gps_pos with
    | None, None -> rellocate_to t pos
    | Some prev_pos, Some prev_gps_pos ->
      let diff = Vec.sub pos prev_pos in
      let error = random_vec (Vec.mag diff *. t.error_per) in
      let gps_pos = Vec.add prev_gps_pos (Vec.add diff error) in
      if Float.(Vec.dist_sq gps_pos pos > t.rellocate_dist ** 2.)
      then rellocate_to t pos
      else gps_pos
    | Some _, None | None, Some _ -> raise (Failure "Unexpected failure in gps logic")
  in
  Rs.set Rs.empty t.gps_pos_sd gps_pos
;;

let est t = Sd_est.create (update_logic t) [ Sd.pack t.gps_pos_sd ]
