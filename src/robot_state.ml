open! Core

(* I think I want this to be mutable *)

type t =
  | Single of Univ_map.t
  (* the first t in the union takes precedence*)
  | Union of t * t

let create () = Single Univ_map.empty
let union t1 t2 = Union (t1, t2)

let rec find t (sd : 'a Sd.t) =
  match t with
  | Single data -> Univ_map.find data sd
  | Union (t1, t2) ->
    (match find t1 sd with
    | Some v -> Some v
    | None -> find t2 sd)
;;

let mem t (sd : 'a Sd.t) = Option.is_some (find t sd)

let rec set t (sd : 'a Sd.t) v =
  match t with
  | Single data -> Single (Univ_map.set data sd v)
  | Union (t1, t2) -> Union (set t1 sd v, t2)
;;

let rec remove t (sd : 'a Sd.t) =
  match t with
  | Single data -> Single (Univ_map.remove data sd)
  | Union (t1, t2) -> Union (remove t1 sd, remove t2 sd)
;;

let memp t (sd_p : Sd.Packed.t) =
  match sd_p with
  | P sd -> mem t sd
;;

let removep t (sd_p : Sd.Packed.t) =
  match sd_p with
  | P sd -> remove t sd
;;

let rec all_datas ?(curr = []) t =
  match t with
  | Single data -> data :: curr
  | Union (t1, t2) ->
    let curr = all_datas ~curr t2 in
    all_datas ~curr t1
;;

let cons_unique xs x ~equal = if List.mem xs x ~equal then xs else x :: xs
let remove_duplicates xs ~equal = List.fold_left ~f:(cons_unique ~equal) ~init:[] xs

let keys t =
  let single =
    match t with
    | Single _ -> true
    | _ -> false
  in
  let univ_map_lists = List.map ~f:Univ_map.to_alist (all_datas t) in
  let univ_map_list =
    List.fold ~init:[] ~f:(fun big single -> single @ big) univ_map_lists
  in
  let unpacked =
    List.map univ_map_list ~f:(fun packed ->
        match packed with
        | T (id, _data) -> Sd.pack id)
  in
  if single then unpacked else remove_duplicates unpacked ~equal:Sd.Packed.equal
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
  | Some sds_to_use ->
    List.fold_left sds_to_use ~init:t1 ~f:(fun t (Sd.Packed.P sd) -> use_sd t t2 sd)
  | None -> List.fold_left (keys t2) ~init:t1 ~f:(fun t sd -> use_sd_p t t2 sd)
;;

let trim_to t sds = use (create ()) ~to_use:(Some sds) t

let rec collapse t =
  match t with
  | Single _data -> t
  | Union (t1, t2) -> use (collapse t2) (collapse t1)
;;
