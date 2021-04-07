open! Core
open Robot_state

module Robot_state_history (SD : SD) = struct
  type t = { states : Robot_state(SD).t Static_deque.t }

  let create ~max_length = { states = Static_deque.create ~max_length }

  let get_state t i = Static_deque.get t.states i

  let get_current_state t = get_state t 0

  let add_state t state = Static_deque.add t.states state

  let copy t = { states = Static_deque.copy t.states }
end
