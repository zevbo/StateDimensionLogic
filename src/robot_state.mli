open! Core

(** SD, or State Dimension. SD.t represents all possible dimensions (or pieces of data) of your robot *)
module type SD = sig 
    type t [@@deriving sexp, hash, compare]
end

module Robot_state(SD: SD) : sig 
    type t

    (** [create ()] creates a [t] with no data *)
    val create : unit -> t

    (** [mem t sd] returns whether or not [t] has data stored for [sd] *)
    val mem : t -> SD.t -> bool

    (** [find t sd] returns [Some] (the current binding) of [sd] in [t], or [None] if no such binding exists. *)
    val find : t -> SD.t -> float option

    (** [set t sd] sets the given [sd] to [t] *)
    val set : t -> SD.t -> float -> unit 

    (** [remove t sd] removes the current binding for [sd] if such binding exists *)
    val remove : t -> SD.t -> unit 

    (** [use t1 ?to_use t2] calls [set t1 (get t2 sd)] for each [sd] where (a) there exists a binding in [t2] and (b) it is in [?to_use] or [?to_use] is [None] *) 
    val use : t -> ?to_use:(Set.Make(SD).t option) -> t -> unit

    (** [use_extras t1 t2] calls [set t1 (get t2 sd)] for each [sd] where (a) there exists a binding in [t2] and (b) there does not exist a binding in [t1] *)
    val use_extras : t -> t -> unit 

    (** [trim_to t sd_set] removes all [sd]s from [t] that are in [sd_set] *)
    val trim_to : t -> Set.Make(SD).t -> unit

    (** [keys t] returns a list of all [sd] for which there exists a binding in [t] *)
    val keys : t -> SD.t list

end
