open Compat;

exception Internal_error string;

exception Evaluation_error string;

type native;

let module Value: {
  type t = Sexp.t native;
  let to_string: t => string;
  let is_proc: t => bool;
  let is_meta: t => bool;
};

let module Code: {type t; let to_string: t => string;};

type t;

let create: unit => t;

let compile: t => Value.t => result Code.t string;

let macroexpand: bool => t => Value.t => result Value.t string;

let eval: t => Value.t => result Value.t string;

let module Exec: {
  type t;
  let push: t => Value.t => unit;
  let apply: t => Value.t => list Value.t => unit;
  let capture_cont: t => Value.t;
};

let context: Exec.t => t;

let register_builtin: string => (Exec.t => list Value.t => unit) => t => unit;
