open! Core
open Src

module F (Sd : Robot_state.Sd) : sig
  val create : Est.F(Sd).t list -> f:(unit -> int) -> Est.F(Sd).t
end