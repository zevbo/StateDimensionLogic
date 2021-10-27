open! Core

type t

(** [create ()] creates a [t] with no data *)
val create : unit -> t

(** [union t1 t2] creates a [t] that combines the data with of [t1] and [t2] giving precedence to the data in [t1] without allocating a new mapping and with O(1) time complexity *)
val union : t -> t -> t

(** [mem t sd] returns whether or not [t] has data stored for [sd] *)
val mem : t -> 'a Sd.t -> bool

val memp : t -> Sd.Packed.t -> bool

(** [find t sd] returns [Some] (the current binding) of [sd] in [t], or [None] if no such binding exists. *)
val find : t -> 'a Sd.t -> 'a option

(** [set t sd v] sets the given [sd] to [v] *)
val set : t -> 'a Sd.t -> 'a -> t

(** [remove t sd] removes the current binding for [sd] if such binding exists *)
val remove : t -> 'a Sd.t -> t

val removep : t -> Sd.Packed.t -> t

(** [use t1 ?to_use t2] calls [set t1 (get t2 sd)] for each [sd] where (a) there exists a binding in [t2] and (b) it is in [?to_use] or [?to_use] is [None] *)
val use : t -> ?to_use:Sd.Packed.t list option -> t -> t

(** [trim_to t sd_set] removes all [sd]s from [t] that are in [sd_set] *)
val trim_to : t -> Sd.Packed.t list -> t

(** [keys t] returns a list of all [sd] for which there exists a binding in [t] *)
val keys : t -> Sd.Packed.t list

(** [collapse t] will make sure your t is represented without a union *)
val collapse : t -> t