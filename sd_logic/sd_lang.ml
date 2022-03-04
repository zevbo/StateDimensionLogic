open! Core

module T = struct
  type 'a default =
    | V of 'a
    | Last
    | Safe_last of 'a
    | Unsafe

  type _ t =
    | Return : 'a -> 'a t
    | Map2 : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c t
    | Sd : 'a Sd.t -> 'a t
    | Sd_past : 'a Sd.t * int * 'a default -> 'a t
    | Sd_history : 'a Sd.t * int -> (int -> 'a option) t
    | State : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> Rs.t t
    | State_past : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t * int -> Rs.t t
    | Full_rsh : unit -> Rsh.t t
end

include T

include Applicative.Make_using_map2 (struct
  include T

  let return x = Return x
  let map2 t1 t2 ~f = Map2 (t1, t2, f)
  let map = `Define_using_map2
end)

let dependency_of_list l = Map.of_alist_reduce (module Sd.Packed) l ~f:max

let rec dependencies
    : type a. a t -> (Sd.Packed.t, int, Sd.Packed.comparator_witness) Map.t
  = function
  | Full_rsh () | Return _ -> Map.empty (module Sd.Packed)
  | Map2 (a, b, _) ->
    Map.merge (dependencies a) (dependencies b) ~f:(fun ~key:_k values ->
        match values with
        | `Both (v1, v2) -> Some (max v1 v2)
        | `Left v1 -> Some v1
        | `Right v2 -> Some v2)
  | Sd sd -> dependency_of_list [ Sd.pack sd, 0 ]
  | Sd_past (sd, n, _default) -> dependency_of_list [ Sd.pack sd, n ]
  | Sd_history (sd, n) -> dependency_of_list [ Sd.pack sd, n ]
  | State sd_set -> Map.of_key_set sd_set ~f:(fun _key -> 0)
  | State_past (sd_set, i) -> Map.of_key_set sd_set ~f:(fun _key -> i)
;;

let dependency_union d1 d2 = Map.merge_skewed d1 d2 ~combine:(fun ~key:_key -> Int.max)

(* -1 implies it doesn't exist period *)
exception Sd_not_found of (string * int)

let rec execute : 'a. 'a t -> Rsh.t -> 'a =
  fun (type a) (t : a t) (rsh : Robot_state_history.t) ->
   match t with
   | Return a -> a
   | Map2 (a, b, f) -> f (execute a rsh) (execute b rsh)
   | Sd sd ->
     (try Rsh.find_exn rsh sd with
     | _ -> raise (Sd_not_found (Sd.Packed.to_string (Sd.pack sd), 0)))
   | Sd_past (sd, n, default) ->
     (match default with
     (* probably wanna change this so it only defaults if we're past the length *)
     | V default ->
       if n >= Rsh.length rsh
       then default
       else (
         try Rsh.find_past_exn rsh n sd with
         | _ -> raise (Sd_not_found (Sd.Packed.to_string (Sd.pack sd), n)))
     | Safe_last default ->
       if n > 0 && Rsh.length rsh <= 1
       then default
       else execute (Sd_past (sd, n, Last)) rsh
     | Last ->
       (try Option.value_exn (Rsh.find_past_last_def rsh n sd) with
       | _ -> raise (Sd_not_found (Sd.Packed.to_string (Sd.pack sd), -1)))
     | Unsafe ->
       (try Option.value_exn (Rsh.find_past rsh n sd) with
       | _ -> raise (Sd_not_found (Sd.Packed.to_string (Sd.pack sd), n))))
   | Sd_history (sd, _size) -> fun i -> Rsh.find_past rsh i sd
   | State sd_set -> Rs.trim_to (Rsh.curr_state rsh) sd_set
   | State_past (sd_set, i) ->
     (match Rsh.nth_state rsh i with
     | None -> Rs.empty
     | Some rs -> Rs.trim_to rs sd_set)
   | Full_rsh () -> rsh
;;

module Let_syntax = struct
  module Let_syntax = struct
    let return = return
    let map = map
    let both = both

    module Open_on_rhs = struct
      let return = return
      let sd x = Sd x
      let sd_past x n def = Sd_past (x, n, def)
      let sd_history x n = Sd_history (x, n)
      let state set = State set
      let state_past set n = State_past (set, n)
      let full_rsh () = Full_rsh ()
    end
  end
end