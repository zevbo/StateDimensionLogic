open! Core
open Src

module F (SD : Robot_state.SD) : sig
    val create : Est.F(SD).t list -> f:(unit -> int) -> Est.F(SD).t
end