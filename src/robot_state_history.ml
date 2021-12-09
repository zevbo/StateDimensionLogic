open! Core

type tick = int

type t =
  { past_states : (tick, Robot_state.t, Int.comparator_witness) Map.t
  ; curr_state : Robot_state.t
  ; tick : tick
  ; max_length : int
  }

let create ~max_length =
  if max_length <= 0
  then
    raise
      (Invalid_argument
         (Printf.sprintf
            "~max_length in Robot_state_history.create must be positive. Given %i"
            max_length));
  { past_states = Map.empty (module Int)
  ; max_length
  ; tick = 0
  ; curr_state = Robot_state.empty
  }
;;

let next_tick t n =
  assert (t.max_length > 1);
  (n + 1) % (t.max_length - 1)
;;

let nth_to_tick t n =
  let t = (t.tick - n) % (t.max_length - 1) in
  t
;;

let nth_state t n =
  if n = 0 then Some t.curr_state else Map.find t.past_states (nth_to_tick t n)
;;

let curr_state t = t.curr_state
let find t sd = Robot_state.find (curr_state t) sd
let find_exn t sd = Robot_state.find_exn (curr_state t) sd

let find_past t n sd =
  match nth_state t n with
  | None -> None
  | Some state -> Robot_state.find state sd
;;

let find_past_def t ~default n sd = Option.value ~default (find_past t n sd)
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

let add_state t =
  if t.max_length = 1
  then { t with curr_state = Rs.empty }
  else (
    let tick = next_tick t t.tick in
    let curr_state = Robot_state.empty in
    let past_states = Map.set t.past_states ~key:t.tick ~data:t.curr_state in
    { t with tick; curr_state; past_states })
;;

let use t ?(to_use = None) (state : Robot_state.t) =
  { t with curr_state = Robot_state.use ~to_use t.curr_state state }
;;

let max_length t = t.max_length

(* TODO: I can make length O(log(n)^2) time complexity *)
let length t = 1 + Map.length t.past_states

type sexpable_rsh =
  { states : Robot_state.t list
  ; max_length : int
  }
[@@deriving sexp_of]

let sexp_of_t t =
  let states = List.filter_map (List.range 0 (length t)) ~f:(fun n -> nth_state t n) in
  sexp_of_sexpable_rsh { states; max_length = t.max_length }
;;