open! Core

module type SD = sig
  type 'a t [@@deriving sexp, hash, compare]
end

(* I think I want this to be mutable *)

module F (SD : SD) = struct
  type t = { data : Univ_map.t }

  let sd_type sexp_of sd = Type_equal.Id.create ~name:(SD.sexp_of_t sexp_of sd) sexp_of
  let create () = { data = Univ_map.empty }
  let umap_f f t sexp_of sd = f t (sd_type sexp_of sd)
  let memt = umap_f Univ_map.mem
  let findt t sexp_of sd = umap_f Univ_map.find t sexp_of sd

  let sett t (sexp_of : 'a -> Sexp.t) (sd : 'a SD.t) data =
    Univ_map.set t.data (sd_type sexp_of sd) data
  ;;

  let removet t (sexp_of : 'a -> Sexp.t) (sd : 'a SD.t) =
    Univ_map.remove t.data (sd_type sexp_of sd)
  ;;

  let mem t (sexp_of : 'a -> Sexp.t) (sd : float SD.t) =
    Univ_map.mem t.data (sd_type sexp_of sd)
  ;;

  let find t = findt t Float.sexp_of_t
  let set t = sett t Float.sexp_of_t
  let remove t = removet t Float.sexp_of_t
  let keys t = Univ_map.to_alist t.data

  let use_sd t1 t2 sd =
    match find t2 sd with
    | Some data -> set t1 sd data
    | None -> ()
  ;;

  let use t1 ?(to_use = None) t2 =
    let use_sd = use_sd t1 t2 in
    match to_use with
    | Some sds_to_use -> Set.iter sds_to_use ~f:use_sd
    | None -> List.iter (keys t2) ~f:use_sd
  ;;

  let use_extras t1 t2 =
    List.iter (keys t2) ~f:(fun sd -> if not (mem t1 sd) then use_sd t1 t2 sd)
  ;;

  let trim_to t sd_set =
    List.iter (keys t) ~f:(fun sd -> if not (Set.mem sd_set sd) then remove t sd)
  ;;
end
