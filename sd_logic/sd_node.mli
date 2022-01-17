open Core_kernel

type t =
  { logic : Robot_state.t Sd_lang.t
  ; sds_estimating : Set.M(Sd.Packed).t
  }

val create : Robot_state.t Sd_lang.t -> Set.M(Sd.Packed).t -> t

type safety =
  | Safe
  | Warnings
  | Unsafe

exception Missing_sd of Sd.Packed.t [@@deriving sexp]
exception Extra_sd of Sd.Packed.t [@@deriving sexp]

val execute : safety:safety -> t -> Rsh.t -> Rs.t
