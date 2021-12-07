open! Core

(* I think I want this to be mutable *)

type t =
  { data : Univ_map.t
  ; sd_map : (Sd.Packed.t Map.M(Type_equal.Id.Uid).t[@sexp.opaque])
  }
(* the first t in the union takes precedence *)
[@@deriving sexp_of]

let empty = { data = Univ_map.empty; sd_map = Map.empty (module Type_equal.Id.Uid) }
let find t (sd : 'a Sd.t) = Univ_map.find t.data (Sd.to_type_equal_id sd)
let find_exn t sd = Option.value_exn (find t sd)
let mem t (sd : 'a Sd.t) = Option.is_some (find t sd)

let set t (sd : 'a Sd.t) v =
  let te = Sd.to_type_equal_id sd in
  { data = Univ_map.set t.data te v
  ; sd_map = Map.set ~key:(Type_equal.Id.uid te) ~data:(Sd.pack sd) t.sd_map
  }
;;

let remove t (sd : 'a Sd.t) =
  let te = Sd.to_type_equal_id sd in
  { data = Univ_map.remove t.data te
  ; sd_map = Map.remove t.sd_map (Type_equal.Id.uid te)
  }
;;

let memp t (sd_p : Sd.Packed.t) =
  match sd_p with
  | P sd -> mem t sd
;;

let removep t (sd_p : Sd.Packed.t) =
  match sd_p with
  | P sd -> remove t sd
;;

let keys t = List.map ~f:snd (Map.to_alist t.sd_map)

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
  | Some sds_to_use ->
    List.fold_left sds_to_use ~init:t1 ~f:(fun t (Sd.Packed.P sd) -> use_sd t t2 sd)
  | None -> List.fold_left (keys t2) ~init:t1 ~f:(fun t sd -> use_sd_p t t2 sd)
;;

let trim_to t sds = use empty ~to_use:(Some sds) t
