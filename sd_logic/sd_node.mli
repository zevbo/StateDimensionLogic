open! Core

type id = int

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

val exit : ([ `Exit ], unit) t
val tick : unit -> ([ `Tick ], child_t) t
val fork : unit -> ([ `Fork ], child_t * child_t) t

val fork_and_waitpid
  :  unit
  -> ([ `Fork ], child_t * child_t) t * ([ `Waitpid ], child_t) t

val est : Sd_est.t -> ([ `Est ], child_t) t
val estl : Rs.t Sd_func.t -> Sd_est.E.t list -> ([ `Est ], child_t) t
val desc : bool Sd_func.t -> ([ `Desc ], child_t * child_t) t
val compare : ('a1, 'a2) t -> ('b1, 'b2) t -> int
val hash : ('a, 'b) t -> int
