open! Core
open Sd_logic
open Sd_lang

type t =
  { error : float
  ; real_pos_sd : float Sd.t
  ; pos_sd : float Sd.t
  }

let create error pos_sd =
  { error; real_pos_sd = pos_sd; pos_sd = Sd.create "encoder_pos" Float.sexp_of_t }
;;

let update_logic t =
  let+ real_pos = sd t.real_pos_sd in
  Rs.set Rs.empty t.pos_sd (real_pos +. Random.float_range (-1.0 *. t.error) t.error)
;;

let est t = Sd_est.create (update_logic t) [ Sd.pack t.pos_sd ]
