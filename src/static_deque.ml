open! Core

type 'a t =
  { data : 'a Deque.t
  ; max_length : int
  }

let create ~max_length =
  { data = Deque.create ~initial_length:max_length ~never_shrink:true (); max_length }
;;

let length t = Deque.length t.data

let get t index =
  try Some (Deque.get t.data index) with
  | _ -> None
;;

let get_last_default t index =
  let eff_index =
    if index >= Deque.length t.data && index < t.max_length
    then Deque.length t.data - 1
    else index
  in
  get t eff_index
;;

let add t el =
  if length t = t.max_length then Deque.drop_back t.data;
  Deque.enqueue_front t.data el
;;

let copy t =
  let new_t = create ~max_length:t.max_length in
  Deque.iter t.data ~f:(fun el -> Deque.enqueue_back new_t.data el);
  new_t
;;

let max_length t = t.max_length
