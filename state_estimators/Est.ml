open! Core
open Src

type t =
  { logic : Robot_state.t Sd_lang.t
  ; sds_estimating : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t
  }

let create logic sds_estimating = { logic; sds_estimating }
