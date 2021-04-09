open! Core

module type SD = sig
  type t [@@deriving sexp, hash, compare]
end

module F (SD : SD) = struct
  type t = { data : (SD.t, float) Hashtbl.t }

  let create () = { data = Hashtbl.create (module SD) }

  let mem t sd = Hashtbl.mem t.data sd

  let find t sd = Hashtbl.find t.data sd

  let set t sd data = Hashtbl.set t.data ~key:sd ~data

  let remove t sd = Hashtbl.remove t.data sd

  let keys t = Hashtbl.keys t.data

  let use_sd t1 t2 sd =
    match find t2 sd with Some data -> set t1 sd data | None -> ()

  let use t1 ?(to_use = None) t2 =
    let use_sd = use_sd t1 t2 in
    match to_use with
    | Some sds_to_use -> Set.iter sds_to_use ~f:use_sd
    | None -> List.iter (keys t2) ~f:use_sd

  let use_extras t1 t2 =
    List.iter (keys t2) ~f:(fun sd -> if not (mem t1 sd) then use_sd t1 t2 sd)

  let trim_to t sd_set =
    List.iter (keys t) ~f:(fun sd ->
        if not (Set.mem sd_set sd) then remove t sd)
end
