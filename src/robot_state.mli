open! Core

type t

(** [create ()] creates a [t] with no data *)
val create : unit -> t

(** [mem t sd] returns whether or not [t] has data stored for [sd] *)
val mem : t -> ('a -> Sexp.t) -> 'a Sd.t -> bool

(** [find t sd] returns [Some] (the current binding) of [sd] in [t], or [None] if no such binding exists. *)
val find : t -> ('a -> Sexp.t) -> 'a Sd.t -> 'a option

(** [set t sd] sets the given [sd] to [t] *)
val set : t -> ('a -> Sexp.t) -> 'a Sd.t -> unit

(** [remove t sd] removes the current binding for [sd] if such binding exists *)
val remove : t -> ('a -> Sexp.t) -> 'a Sd.t -> unit

(** [use t1 ?to_use t2] calls [set t1 (get t2 sd)] for each [sd] where (a) there exists a binding in [t2] and (b) it is in [?to_use] or [?to_use] is [None] *)
val use : t -> ?to_use:Sd.Packed.t list option -> t -> unit

(** [trim_to t sd_set] removes all [sd]s from [t] that are in [sd_set] *)
val trim_to : t -> Sd.Packed.t list -> unit

(** [keys t] returns a list of all [sd] for which there exists a binding in [t] *)
val keys : t -> Sd.Packed.t list
