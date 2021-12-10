open! Core

type tick = int

type t =
  { past_states : (tick, Robot_state.t, Int.comparator_witness) Map.t
  ; curr_state : Robot_state.t
  ; tick : tick
  ; sd_categories : Sd.Packed.t list array
  ; max_length : int
  }

(* 1 -> 1, 2 -> 2, 3 -> 2, 7 -> 3 *)
let num_categories len = Int.ceil_log2 (len + 1)

(* cateogry 0 -> 0, category 1 -> 2, category 2 -> 6, category 3 -> 14  *)
let max_category_i category = Int.pow 2 (category + 1) - 2

(* 1 -> category 0, 2 -> category 1, 3 -> category 1, 4 -> category 2, 7 -> category 2, 8 -> category 3 *)
let len_to_category i = Int.floor_log2 i
let real_store_len len = max_category_i (len_to_category len) + 1

(* note: for the moment, default_length only matters if it's the max_length *)
let create ?(default_length = 1) ?(sd_lengths = Map.empty (module Sd.Packed)) () =
  if default_length <= 0
  then
    raise
      (Invalid_argument
         (Printf.sprintf
            "~default_length in Robot_state_history.create must be positive. Given %i"
            default_length));
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
         (default_length :: List.map ~f:snd (Map.to_alist sd_lengths)))
  in
  let sd_categories = Array.create ~len:(num_categories max_length) [] in
  Map.iteri sd_lengths ~f:(fun ~key ~data ->
      let category = len_to_category data in
      Array.set sd_categories category (key :: Array.get sd_categories category));
  { past_states = Map.empty (module Int)
  ; tick = 0
  ; curr_state = Robot_state.empty
  ; sd_categories
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
  if n = 0 then Some t.curr_state else Map.find t.past_states (nth_to_tick t n)
;;

let max_length t = t.max_length

(* TODO: I can make length O(log(n)^2) time complexity *)
let length t = 1 + Map.length t.past_states
let curr_state t = t.curr_state
let find t sd = Robot_state.find (curr_state t) sd
let find_exn t sd = Robot_state.find_exn (curr_state t) sd

let find_past t n sd =
  match nth_state t n with
  | None -> None
  | Some state -> Robot_state.find state sd
;;

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
    let l = length t in
    let categories = List.range 0 (num_categories l) in
    let trimmed =
      List.fold_left
        categories
        ~f:(fun map category ->
          let i = max_category_i category + 1 in
          if i >= l
          then map
          else (
            let tick = nth_to_tick t ~tick i in
            let state = Map.find_exn map tick in
            let sds_to_delete = Array.get t.sd_categories category in
            let state =
              List.fold sds_to_delete ~f:(fun state sd -> Rs.removep state sd) ~init:state
            in
            Map.set map ~key:tick ~data:state))
        ~init:past_states
    in
    { t with tick; curr_state; past_states = trimmed })
;;

let use t ?(to_use = None) (state : Robot_state.t) =
  { t with curr_state = Robot_state.use ~to_use t.curr_state state }
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