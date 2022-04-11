open! Core

type id = int

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

val create : 'a info -> 'a t
val exit : unit t
val tick : unit -> child_t t
val fork : unit -> (child_t * child_t) t
val est : Sd_est.t -> child_t t
val estl : Rs.t Sd_lang.t -> Sd.Packed.t list -> child_t t
val desc : bool Sd_lang.t -> (child_t * child_t) t
val compare : 'a t -> 'b t -> int
val hash : 'a t -> int
