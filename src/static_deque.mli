type 'a t

val create : max_length:int -> 'a t

val get : 'a t -> int -> 'a option
val add : 'a t -> 'a -> unit
val length : 'a t -> int 
val max_length : 'a t -> int
val copy : 'a t -> 'a t