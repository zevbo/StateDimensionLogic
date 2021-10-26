open! Core

(* I think I want this to be mutable *)

type t = { data : Univ_map.t }

let create () = { data = Univ_map.empty }
let mem t (sd : 'a Sd.t) = Univ_map.mem t.data sd
let find t (sd : 'a Sd.t) = Univ_map.find t.data sd
let set t (sd : 'a Sd.t) data = { data = Univ_map.set t.data sd data }
let remove t (sd : 'a Sd.t) = { data = Univ_map.remove t.data sd }

let memp t (sd_p : Sd.Packed.t) =
  match sd_p with
  | P sd -> Univ_map.mem t.data sd
;;

let removep t (sd_p : Sd.Packed.t) =
  match sd_p with
  | P sd -> { data = Univ_map.remove t.data sd }
;;

let keys t =
  let univ_map_list = Univ_map.to_alist t.data in
  List.map univ_map_list ~f:(fun packed ->
      match packed with
      | T (id, _data) -> Sd.pack id)
;;

let use_sd t1 t2 sd =
  match find t2 sd with
  | Some data -> set t1 sd data
  | None -> t1
;;

let use_sd_p t1 t2 (packed : Sd.Packed.t) =
  match packed with
  | P sd ->
    (match find t2 sd with
    | Some data -> set t1 sd data
    | None -> t1)
;;

let use t1 ?(to_use = None) t2 =
  match to_use with
  | Some sds_to_use -> Set.fold sds_to_use ~init:t1 ~f:(fun t sd -> use_sd t t2 sd)
  | None -> List.fold_left (keys t2) ~init:t1 ~f:(fun t sd -> use_sd_p t t2 sd)
;;

(*

  let use_extras t1 t2 =
    List.iter (keys t2) ~f:(fun sd -> if not (mem t1 sd) then use_sd t1 t2 sd)
  ;;
  *)

let trim_to t sds = use (create ()) ~to_use:sds t
