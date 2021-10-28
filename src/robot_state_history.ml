open! Core

type tick = int

type t =
  { past_states : (tick, Robot_state.t, Int.comparator_witness) Map.t
  ; curr_state : Robot_state.t
  ; tick : tick
  ; max_length : int
  }

let create ~max_length =
  { past_states = Map.empty (module Int)
  ; max_length
  ; tick = 0
  ; curr_state = Robot_state.create ()
  }
;;

let next_tick t n = (n + 1) % t.max_length
let nth_to_tick t n = (t.tick - n) % t.max_length

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
  let tick = next_tick t t.tick in
  let curr_state = Robot_state.create () in
  let past_states = Map.set t.past_states ~key:t.tick ~data:t.curr_state in
  { t with tick; curr_state; past_states }
;;

let use t (state : Robot_state.t) =
  { t with curr_state = Robot_state.use t.curr_state state }
;;

let max_length t = t.max_length

(* TODO: I can make length O(log(n)^2) time complexity *)
let length t = 1 + Map.length t.past_states