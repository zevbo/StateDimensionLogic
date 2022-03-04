open! Sd_logic
open! Core

let%test "max" =
  let safety = Safety_level.max Safety_level.Safe Safety_level.Unsafe in
  Safety_level.compare safety Safety_level.Safe = 0
;;
