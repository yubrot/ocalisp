type 'a t

exception UndefinedVariable of string

val create : 'a t option -> 'a t

val def : string -> 'a -> 'a t -> unit
val set : string -> 'a -> 'a t -> unit
val find : string -> 'a t -> 'a option
val get : string -> 'a t -> 'a
