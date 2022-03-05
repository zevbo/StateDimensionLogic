open! Core

type id = int

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

val create : 'a info -> 'a t
val exit : unit t
val tick : unit -> pt t
val fork : unit -> (pt * pt) t
val est : Sd_est.t -> pt t
val desc : bool Sd_lang.t -> (pt * pt) t
val compare : 'a t -> 'b t -> int
val hash : 'a t -> int
