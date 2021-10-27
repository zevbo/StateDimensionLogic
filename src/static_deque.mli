type 'a t

val create : max_length:int -> 'a t
val get : 'a t -> int -> 'a option

(** like get, except if the index is both less than max_length, but out of range of the deque, it will attempt to get the last value of the deque *)
val get_last_default : 'a t -> int -> 'a option

val add : 'a t -> 'a -> unit
val length : 'a t -> int
val max_length : 'a t -> int
val copy : 'a t -> 'a t