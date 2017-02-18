type +'a t =
  | Num of float
  | Sym of string
  | Str of string
  | Cons of 'a t * 'a t
  | Nil
  | True
  | False
  | Pure of 'a

val of_list : 'a t list -> 'a t
val of_bool : bool -> 'a t

val to_list : 'a t -> 'a t list option
val to_bool : 'a t -> bool

val quote : 'a t -> 'a t
val quasiquote : 'a t -> 'a t
val unquote : 'a t -> 'a t
val unquote_splicing : 'a t -> 'a t

val to_string : ('a -> string) -> 'a t -> string
