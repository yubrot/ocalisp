exception Internal_error of string
exception Evaluation_error of string

type native

module Value : sig
  type t = native Sexp.t

  val to_string : t -> string
  val is_proc : t -> bool
  val is_meta : t -> bool
end

type t

val create : unit -> t
val macroexpand : bool -> t -> Value.t -> Value.t
val eval : t -> Value.t -> (Value.t, string) result

module Exec : sig
  type t

  val push : t -> Value.t -> unit
  val apply : t -> Value.t -> Value.t list -> unit
  val capture_cont : t -> Value.t
end

val context : Exec.t -> t

val register_builtin : string -> (Exec.t -> Value.t list -> unit) -> t -> unit
