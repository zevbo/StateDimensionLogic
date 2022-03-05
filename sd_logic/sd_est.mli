open Core_kernel

type t =
  { logic : Robot_state.t Sd_lang.t
  ; sds_estimating : Set.M(Sd.Packed).t
  }

val create : Robot_state.t Sd_lang.t -> Set.M(Sd.Packed).t -> t

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