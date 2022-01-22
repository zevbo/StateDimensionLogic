open! Core

type t =
  | Unsafe
  | Warnings
  | Safe
[@@deriving compare]

let max t1 t2 = if compare t1 t2 > 0 then t1 else t2