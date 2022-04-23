open! Sd_logic
open! Core

let pos = Sd.create "pos" Vec.sexp_of_t
let angle = Sd.create "angle" Float.sexp_of_t
let lomega = Sd.create "lomega" Float.sexp_of_t
let romega = Sd.create "romega" Float.sexp_of_t
let linput = Sd.create "linput" Float.sexp_of_t
let rinput = Sd.create "rinput" Float.sexp_of_t