open! Core

module type SD = sig 
    type t
    val t_of_sexp : Sexplib0__.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0__.Sexp.t
    val compare : t -> t -> int
    val hash : t -> int
end

module Robot_state(SD: SD) : sig 
    type t

    val create : unit -> t

    val mem : t -> SD.t -> bool

    val get : t -> SD.t -> float option
    val set : t -> SD.t -> float -> unit 
    val remove : t -> SD.t -> unit 

    val use : t -> ?to_use:(SD.t list option) -> t -> unit
    val use_extras : t -> t -> unit 
    val trim_to : t -> Set.Make(SD).t -> unit

    val keys : t -> SD.t list

end
