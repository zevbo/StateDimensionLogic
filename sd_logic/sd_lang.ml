open! Core

module T = struct
  type ('a, _) default =
    | V : 'a -> ('a, 'a) default (* in case of too few states, return associated value of type 'a *)
    | Last : ('a, 'a) default (* in case of too few states, use the oldest state *)
    | Safe_last : ('a, 'a option) default (* like last, except in case of too few states and only current state exists, use None *)
    | Unsafe : ('a, 'a) default
    | Op : ('a, 'a option) default

  type _ t =
    | Return : 'a -> 'a t
    | Map2 : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c t
    | Sd : 'a Sd.t -> 'a t
    | Sd_past : 'a Sd.t * int * ('a, 'b) default -> 'b t
    | Sd_history : 'a Sd.t * int -> (int -> 'a option) t
    | State : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> Rs.t t
    | State_past : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t * int -> Rs.t t
    | Full_rsh : unit -> Rsh.t t

  type packed = P : 'a t -> packed
end

include T

include Applicative.Make_using_map2 (struct
  include T

  let return x = Return x
  let map2 t1 t2 ~f = Map2 (t1, t2, f)
  let map = `Define_using_map2
end)

let sdependency_union = Int.max
let sdependency_w_curr_union (v1, b1) (v2, b2) = sdependency_union v1 v2, b1 || b2

let dependency_w_curr_of_list l =
  Map.of_alist_reduce (module Sd.Packed) l ~f:sdependency_w_curr_union
;;

let dependency_w_curr_union d1 d2 =
  Map.merge_skewed d1 d2 ~combine:(fun ~key:_key -> sdependency_w_curr_union)
;;

let dependency_union d1 d2 =
  Map.merge_skewed d1 d2 ~combine:(fun ~key:_key -> sdependency_union)
;;

(* should have sense of required and optional for current *)
let rec dependencies_w_curr_p
    : packed -> (Sd.Packed.t, int * bool, Sd.Packed.comparator_witness) Map.t
  = function
  | P t ->
    (match t with
    | Full_rsh () | Return _ -> Map.empty (module Sd.Packed)
    | Map2 (a, b, _) ->
      dependency_w_curr_union (dependencies_w_curr_p (P a)) (dependencies_w_curr_p (P b))
    | Sd sd -> dependency_w_curr_of_list [ Sd.pack sd, (0, true) ]
    | Sd_past (sd, n, _default) -> dependency_w_curr_of_list [ Sd.pack sd, (n, n = 0) ]
    | Sd_history (sd, n) -> dependency_w_curr_of_list [ Sd.pack sd, (n, false) ]
    | State sd_set -> Map.of_key_set sd_set ~f:(fun _key -> 0, false)
    | State_past (sd_set, i) -> Map.of_key_set sd_set ~f:(fun _key -> i, false))
;;

let dependencies t = Map.map (dependencies_w_curr_p (P t)) ~f:fst
let curr_req t = Map.key_set (Map.filter (dependencies_w_curr_p (P t)) ~f:snd)

(* -1 implies it doesn't exist period *)
exception Sd_not_found of (Sd.Packed.t * int) [@@deriving sexp]

let rec execute : 'a. 'a t -> Rsh.t -> 'a =
  fun (type a) (t : a t) (rsh : Robot_state_history.t) ->
   match t with
   | Return a -> a
   | Map2 (a, b, f) -> f (execute a rsh) (execute b rsh)
   | Sd sd ->
     (try Rsh.find_exn rsh sd with
     | _ -> raise (Sd_not_found (Sd.pack sd, 0)))
   | Sd_past (sd, n, default) ->
     (match default with
     | V default ->
       if n >= Rsh.length rsh
       then default
       else Option.value (Rsh.find_past rsh n sd) ~default
     | Safe_last ->
       if n > 0 && Rsh.length rsh <= 1
       then None
       else Some (execute (Sd_past (sd, n, Last)) rsh)
     | Last ->
       (try Option.value_exn (Rsh.find_past_last_def rsh n sd) with
       | _ -> raise (Sd_not_found (Sd.pack sd, -1)))
     | Unsafe ->
       (try Option.value_exn (Rsh.find_past rsh n sd) with
       | _ -> raise (Sd_not_found (Sd.pack sd, n)))
     | Op -> Rsh.find_past rsh n sd)
   | Sd_history (sd, _size) -> fun i -> Rsh.find_past rsh i sd
   | State sd_set -> Rs.trim_to (Rsh.curr_state rsh) sd_set
   | State_past (sd_set, i) ->
     (match Rsh.nth_state rsh i with
     | None -> Rs.empty
     | Some rs -> Rs.trim_to rs sd_set)
   | Full_rsh () -> rsh
;;

let ( let+ ) a f = map a ~f
let ( and+ ) = both
let return = return
let sd x = Sd x
let sd_past x n def = Sd_past (x, n, def)
let sd_history x n = Sd_history (x, n)
let state set = State set
let state_past set n = State_past (set, n)
let full_rsh () = Full_rsh ()
