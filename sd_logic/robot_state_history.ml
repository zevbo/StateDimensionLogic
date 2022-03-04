open! Core

type tick = int

type t =
  { past_states : (tick, Robot_state.t, Int.comparator_witness) Map.t
  ; curr_state : Robot_state.t
  ; tick : tick
  ; lengths_to_sds : (int, Sd.Packed.t list, Int.comparator_witness) Map.t
  ; max_length : int
  }

(* note: for the moment, default_length only matters if it's the max_length *)
let create ?(sd_lengths = Map.empty (module Sd.Packed)) ?(min_default_length = 1) () =
  if min_default_length <= 0
  then
    raise
      (Invalid_argument
         (Printf.sprintf
            "~min_default_length in Robot_state_history.create must be positive. Given %i"
            min_default_length));
  Map.iteri sd_lengths ~f:(fun ~key ~data ->
      if data <= 0
      then
        raise
          (Invalid_argument
             (Printf.sprintf
                "entries in ~sd_lengths in Robot_state_history.create must be positive. \
                 Given %i for key %s"
                data
                (Sd.Packed.to_string key))));
  let max_length =
    Option.value_exn
      (List.max_elt
         ~compare:Int.compare
         (min_default_length :: List.map ~f:snd (Map.to_alist sd_lengths)))
  in
  let lengths_to_sds =
    Map.fold
      sd_lengths
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data map ->
        Map.set map ~key:data ~data:(key :: Option.value (Map.find map data) ~default:[]))
  in
  { past_states = Map.empty (module Int)
  ; tick = 0
  ; curr_state = Robot_state.empty
  ; lengths_to_sds
  ; max_length
  }
;;

let next_tick t n =
  assert (t.max_length > 1);
  (n + 1) % (t.max_length - 1)
;;

let nth_to_tick t ?(tick = t.tick) n =
  let t = (tick - n) % (t.max_length - 1) in
  t
;;

let nth_state t n =
  if n = 0
  then Some t.curr_state
  else if n < 0 || n >= t.max_length
  then None
  else Map.find t.past_states (nth_to_tick t n)
;;

let max_length t = t.max_length
let default_length = max_length

(* TODO: I can make length O(log(n)^2) time complexity *)
let length t = 1 + Map.length t.past_states
let curr_state t = t.curr_state
let find t sd = Robot_state.find (curr_state t) sd
let find_exn t sd = Robot_state.find_exn (curr_state t) sd

(* zTODO: should this return an 'a option option? *)
let find_past t n sd =
  match nth_state t n with
  | None -> None
  | Some state -> Robot_state.find state sd
;;

let find_past_exn t n sd = Option.value_exn (find_past t n sd)
let find_past_def t ~default n sd = Option.value ~default (find_past t n sd)
let find_past_last_def t n sd = find_past t (min n (length t - 1)) sd
let mem t sd = Robot_state.mem (curr_state t) sd

let mem_past t n sd =
  match nth_state t n with
  | None -> None
  | Some state -> Some (Robot_state.mem state sd)
;;

let memp t sd = Robot_state.memp (curr_state t) sd

let memp_past t n sd =
  match nth_state t n with
  | None -> None
  | Some state -> Some (Robot_state.memp state sd)
;;

let add_empty_state t =
  if t.max_length = 1
  then { t with curr_state = Rs.empty }
  else (
    let tick = next_tick t t.tick in
    let curr_state = Robot_state.empty in
    let past_states = Map.set t.past_states ~key:t.tick ~data:t.curr_state in
    (* length is 1 we do 0, length is 2 we do 1, length is 3 we do 1, length is 4 we do 2 *)
    (* note: possible imporvement in runtime can be gotten by only deleting values every power of 2, goes from 2n map sets/deletions, to n + min (log max_length, n) *)
    let trimmed =
      List.fold_left
        (List.range 1 (length t))
        ~f:(fun map i ->
          let tick = nth_to_tick t ~tick i in
          let state = Map.find_exn map tick in
          let sds_to_delete_op = Map.find t.lengths_to_sds i in
          match sds_to_delete_op with
          | None -> map
          | Some sds_to_delete ->
            let state =
              List.fold sds_to_delete ~f:(fun state sd -> Rs.removep state sd) ~init:state
            in
            Map.set map ~key:tick ~data:state)
        ~init:past_states
    in
    { t with tick; curr_state; past_states = trimmed })
;;

let use t ?(to_use = None) (state : Robot_state.t) =
  { t with curr_state = Robot_state.use ~to_use t.curr_state state }
;;

let use_extras t (state : Robot_state.t) =
  { t with curr_state = Robot_state.use_extras t.curr_state state }
;;

let add_state t rs = use (add_empty_state t) rs

type sexpable_rsh =
  { states : Robot_state.t list
  ; max_length : int
  }
[@@deriving sexp_of]

let sexp_of_t t =
  let states = List.filter_map (List.range 0 (length t)) ~f:(fun n -> nth_state t n) in
  sexp_of_sexpable_rsh { states; max_length = t.max_length }
;;