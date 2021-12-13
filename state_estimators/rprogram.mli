open! Core
open Src

module type Model = sig
  type t

  val apply : Rsh.t -> t -> Rsh.t
  val sd_lengths : t -> (Sd.Packed.t, int, Sd.Packed.comparator_witness) Map.t
end

(* zTODO: maybe add some form of delay? *)
module M (Model : Model) : sig
  type t

  val create : Model.t -> t
  val tick : t -> t
  val run : t -> ticks:int option -> unit
end