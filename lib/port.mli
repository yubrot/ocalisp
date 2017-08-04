type t

val of_in : in_channel -> t
val of_out : out_channel -> t

val close : t -> unit

val to_in : t -> in_channel option
val to_out : t -> out_channel option
