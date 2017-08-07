exception Internal_error of string
exception Evaluation_error of string

type native

module Value : sig
  type t = native Sexp.t

  val to_string : t -> string

  val of_port : Port.t -> t
  val to_port : t -> Port.t option

  val of_vec : t array -> t
  val to_vec : t -> t array option

  val is_proc : t -> bool
  val is_meta : t -> bool
  val is_port : t -> bool
  val is_vec : t -> bool
end

module Code : sig
  type t
  val to_string : t -> string
end

type t

val create : unit -> t
val compile : t -> Value.t -> (Code.t, string) result
val macroexpand : bool -> t -> Value.t -> (Value.t, string) result
val eval : t -> Value.t -> (Value.t, string) result

module Exec : sig
  type t

  val push : t -> Value.t -> unit
  val apply : t -> Value.t -> Value.t list -> unit
  val capture_cont : t -> Value.t
end

val context : Exec.t -> t

val register_builtin : string -> (Exec.t -> Value.t list -> unit) -> t -> unit
