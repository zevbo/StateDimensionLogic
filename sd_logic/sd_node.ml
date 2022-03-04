open! Core

type id = int

let global_id = ref (-1)

let get_id () =
  global_id := !global_id + 1;
  !global_id
;;

type _ info =
  | Exit : unit info
  | Tick : pt info
  | Fork : (pt * pt) info (* first t is next, second is forked *)
  | Est : Sd_est.t -> pt info
  | Desc : bool Sd_lang.t -> (pt * pt) info

and 'a t =
  { info : 'a info [@hash.ignore] [@compare.ignore]
  ; id : id
  }

and pt = P : _ t -> pt

type conn = Conn : ('a t * 'a) -> conn

let create info = { info; id = get_id () }
let exit = create Exit
let tick () = create Tick
let fork () = create Fork
let est est = create (Est est)
let desc desc = create (Desc desc)
let compare t1 t2 = Int.compare t1.id t2.id
let hash t = Int.hash t.id
