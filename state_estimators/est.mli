open Src
open Core_kernel

type t =
  { logic : Robot_state.t Sd_lang.t
  ; sds_estimating : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t
  }

val create
  :  Robot_state.t Sd_lang.t
  -> (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t
  -> t

type safety =
  | Safe
  | Warnings
  | Unsafe

exception Missing_sd of Sd.Packed.t
exception Extra_sd of string

val execute : safety:safety -> t -> Rsh.t -> Rs.t