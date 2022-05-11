open Core_kernel

module E : sig
  type t =
    | Eq : ('a Sd.t * ('a -> 'a -> bool)) -> t
    | Reg : 'a Sd.t -> t

  type comparator_witness
end

type t =
  { logic : Robot_state.t Sd_func.t
  ; sds_estimating : Set.M(E).t
  ; is_prim : bool
  }

val create_set : ?unstable:bool -> Robot_state.t Sd_func.t -> Set.M(E).t -> t
val create : ?unstable:bool -> Robot_state.t Sd_func.t -> E.t list -> t
val sds_estimating_set : t -> Set.M(Sd.Packed).t
val sd : 'a Sd.t -> E.t
val eq_sd : 'a Sd.t -> ('a -> 'a -> bool) -> E.t

type safety

exception Missing_sd of Sd.Packed.t [@@deriving sexp]
exception Extra_sd of Sd.Packed.t [@@deriving sexp]

val create_safety
  :  ?default:Safety_level.t
  -> ?missing_sd:Safety_level.t
  -> ?extra_sd:Safety_level.t
  -> unit
  -> safety

val execute : safety:safety -> t -> Rsh.t -> Rs.t