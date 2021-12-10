open! Core
open Src

module type Model = sig
  type t

  val apply : Rsh.t -> t -> Rsh.t
end

(* zTODO: maybe add some form of delay? *)
module M (Model : Model) : sig
  type t

  val create : Model.t -> unit Sd_lang.t -> t
  val tick : t -> t
  val run : t -> ticks:int option -> unit
end