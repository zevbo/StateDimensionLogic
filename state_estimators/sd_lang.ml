open! Core
open Src

module T = struct
  type 'a default =
    | Last
    | V of 'a

  type _ t =
    | Return : 'a -> 'a t
    | Map2 : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c t
    | Sd : 'a Sd.t -> 'a t
    | Sd_past : 'a Sd.t * int * 'a default -> 'a t
    | Sd_history : 'a Sd.t * int -> (int -> 'a option) t
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
;;

let rec execute : 'a. Rsh.t -> 'a t -> 'a =
  fun (type a) (rsh : Robot_state_history.t) (t : a t) ->
   match t with
   | Return a -> a
   | Map2 (a, b, f) -> f (execute rsh a) (execute rsh b)
   | Sd sd -> Rsh.find_exn rsh sd
   | Sd_past (sd, n, default) ->
     (match default with
     | V default -> Rsh.find_past_def rsh n sd ~default
     | Last -> Option.value_exn (Rsh.find_past_last_def rsh n sd))
   | Sd_history (sd, _size) -> fun i -> Rsh.find_past rsh i sd
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
      let full_rsh () = Full_rsh ()
    end
  end
end
