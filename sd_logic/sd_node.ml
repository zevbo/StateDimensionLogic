open! Core

type id = int

let global_id = Atomic.make 0
let get_id () = Atomic.fetch_and_add global_id 1

type (_, _) info =
  | Exit : ([ `Exit ], unit) info
  | Tick : ([ `Tick ], child_t) info
  | Fork : ([ `Fork ], child_t * child_t) info (* first t is next, second is forked *)
  | Est : Sd_est.t -> ([ `Est ], child_t) info
  | Desc : bool Sd_func.t -> ([ `Desc ], child_t * child_t) info
  | Waitpid : ([ `Fork ], child_t * child_t) t -> ([ `Waitpid ], child_t) info

and ('a, 'b) t =
  { info : ('a, 'b) info [@hash.ignore] [@compare.ignore]
  ; id : id
  }

and child_t = C : _ t -> child_t

type conn = Conn : (('a, 'b) t * 'b) -> conn

let create info = { info; id = get_id () }
let exit = create Exit
let tick () = create Tick
let fork () = create Fork

let fork_and_waitpid () =
  let f = create Fork in
  f, create (Waitpid f)
;;

let est est = create (Est est)
let estl logic estimating = create (Est (Sd_est.create logic estimating))
let desc desc = create (Desc desc)
let compare t1 t2 = Int.compare t1.id t2.id
let hash t = Int.hash t.id
