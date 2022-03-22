open! Core

type id = int

let global_id = Atomic.make 0
let get_id () = Atomic.fetch_and_add global_id 1

type _ info =
  | Exit : unit info
  | Tick : child_t info
  | Fork : (child_t * child_t) info (* first t is next, second is forked *)
  | Est : Sd_est.t -> child_t info
  | Desc : bool Sd_lang.t -> (child_t * child_t) info

and 'a t =
  { info : 'a info [@hash.ignore] [@compare.ignore]
  ; id : id
  }

and child_t = C : _ t -> child_t

type conn = Conn : ('a t * 'a) -> conn

let create info = { info; id = get_id () }
let exit = create Exit
let tick () = create Tick
let fork () = create Fork
let est est = create (Est est)
let desc desc = create (Desc desc)
let compare t1 t2 = Int.compare t1.id t2.id
let hash t = Int.hash t.id
