open! Core

type t =
  | Unsafe
  | Warnings
  | Safe
[@@deriving compare]

(** returns the safer of the two ts *)
val max : t -> t -> t