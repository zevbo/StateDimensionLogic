open Core_kernel

module Equal_sd : sig
  type t = E : ('a Sd.t * ('a -> 'a -> bool)) -> t

  val create : 'a Sd.t -> ('a -> 'a -> bool) -> t

  type comparator_witness
end

type sds_estimating =
  | Reg of Set.M(Sd.Packed).t
  | Reactive of Set.M(Equal_sd).t

type t =
  { logic : Robot_state.t Sd_func.t
  ; sds_estimating : sds_estimating
  ; signal : bool
  }

val create_set : ?signal:bool -> Robot_state.t Sd_func.t -> Set.M(Sd.Packed).t -> t
val create : ?signal:bool -> Robot_state.t Sd_func.t -> Sd.Packed.t list -> t
val create_reactive : Robot_state.t Sd_func.t -> Equal_sd.t list -> signal:bool -> t
val sds_estimating_set : t -> Set.M(Sd.Packed).t

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