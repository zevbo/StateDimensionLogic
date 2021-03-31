open! Core

type 'a t = { data : 'a Deque.t; max_length : int }

let create ~max_length =
  {
    data = Deque.create ~initial_length:max_length ~never_shrink:true ();
    max_length;
  }

let max_length t = Array.length t.data

let to_real_index t eff_index =
  (t.min_index.contents + eff_index) % max_length t

let to_eff_index t real_index =
  (real_index - t.min_index.contents) % max_length t

let length t =
  to_eff_index t t.max_index.contents - to_eff_index t t.min_index.contents

let normalize_index t index = index % max_length t

let get t eff_index =
  try t.data.(to_real_index t eff_index)
  with Invalid_argument _str ->
    raise
      (Invalid_argument
         (sprintf "Index out of bounds. Index: %d, Length: %d" eff_index
            (length t)))

let change_index real_index inc = real_index + if inc then 1 else -1

let inc_index real_index = change_index real_index true

let dec_index real_index = change_index real_index false

let add t el =
  t.min_index := dec_index t.min_index.contents;
  if length t > max_length t then t.max_index := dec_index t.max_index.contents;
  t.data.(t.min_index.contents) <- el

let copy t =
  { data = Array.copy t.data; min_index = t.min_index; max_index = t.max_index }
