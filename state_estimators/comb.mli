open! Core
open Src

module A : Robot_state.SD -> sig
    type t [@@deriving sexp, hash, compare]
end

module Comb (SD : Robot_state.SD) : sig
    type t 

    val create : (module Est.Est) -> 'a -> t

end