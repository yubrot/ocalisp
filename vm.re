open Compat;

exception Internal_error string;

exception Evaluation_error string;

type code = list inst
and inst =
  | Ldc value
  | Ldv string
  | Ldf pattern code
  | Ldm pattern code
  | Ldb string
  | Sel code code
  | App int
  | Leave
  | Pop
  | Def string
  | Set string
and pattern = {fixed: list string, rest: option string}
and value = Sexp.t native
and native =
  | Fun env pattern code
  | Builtin builtin
  | Macro env pattern code
  | Syntax syntax
and builtin = {run: state => list value => unit}
and syntax = {
  expand: (value => value) => list value => list value,
  compile: (value => code) => list value => code
}
and state = {
  mutable stack: list value,
  mutable env: env,
  mutable code: code,
  mutable dump: list (env, code),
  context: t
}
and env = Env.t value
and t = {toplevel: env, builtins: Hashtbl.t string builtin};

let module Value = {
  type t = value;
  let to_string =
    Sexp.to_string @@ (
      fun
      | Fun _ => "<fun>"
      | Builtin _ => "<builtin>"
      | Macro _ => "<macro>"
      | Syntax _ => "<syntax>"
    );
  let is_proc =
    fun
    | Sexp.Pure (Fun _ | Builtin _) => true
    | _ => false;
  let is_meta =
    fun
    | Sexp.Pure (Macro _ | Syntax _) => true
    | _ => false;
  let on_env env =>
    fun
    | Sexp.Sym sym => Env.find sym env
    | _ => None;
};

let module Pattern = {
  type t = pattern;
  let to_string {fixed, rest} => {
    let fixed = List.map (fun sym => Sexp.Sym sym) fixed;
    let rest =
      switch rest {
      | Some s => Sexp.Sym s
      | None => Sexp.Nil
      };
    Value.to_string (List.fold_right (fun a b => Sexp.Cons a b) fixed rest)
  };
  let rec build =
    fun
    | Sexp.Sym sym => {fixed: [], rest: Some sym}
    | Sexp.Nil => {fixed: [], rest: None}
    | Sexp.Cons a b =>
      switch a {
      | Sexp.Sym sym =>
        let {fixed, rest} = build b;
        {fixed: [sym, ...fixed], rest}
      | a => raise (Evaluation_error ("Unsupported pattern: " ^ Value.to_string a))
      }
    | s => raise (Evaluation_error ("Unsupported pattern: " ^ Value.to_string s));
  let bind pattern args env => {
    let argument_error prefix => raise (
      Evaluation_error (
        "This function takes " ^ prefix ^ string_of_int (List.length pattern.fixed) ^ " arguments"
      )
    );
    let rec bind_fixed =
      fun
      | ([p, ...pattern], [arg, ...args]) => {
          Env.def p arg env;
          bind_fixed (pattern, args)
        }
      | ([], args) => args
      | (_, []) => argument_error "at least ";
    switch (pattern.rest, bind_fixed (pattern.fixed, args)) {
    | (Some k, rest_args) => Env.def k (Sexp.of_list rest_args) env
    | (None, []) => ()
    | (None, rest_args) => argument_error ""
    }
  };
};

let module Code = {
  type t = code;
  let to_string code => {
    let block_id = ref 0;
    let blocks = ref [];
    let rec inst_to_string =
      fun
      | Ldc c => "ldc " ^ Value.to_string c
      | Ldv v => "ldv " ^ v
      | Ldf pattern code => "ldf " ^ add_code ("fun " ^ Pattern.to_string pattern) code
      | Ldm pattern code => "ldm " ^ add_code ("macro " ^ Pattern.to_string pattern) code
      | Ldb s => "ldb " ^ s
      | Sel a b => {
          let a = add_code "then" a;
          let b = add_code "else" b;
          "sel " ^ a ^ " " ^ b
        }
      | App n => "app " ^ string_of_int n
      | Leave => "leave"
      | Pop => "pop"
      | Def s => "def " ^ s
      | Set s => "set " ^ s
    and add_code header code => {
      let id = "[" ^ string_of_int !block_id ^ " " ^ header ^ "]";
      let buf = Buffer.create 0;
      block_id := succ !block_id;
      blocks := [buf, ...!blocks];
      Buffer.add_string buf (id ^ "\n");
      List.iter (fun i => Buffer.add_string buf ("  " ^ inst_to_string i ^ "\n")) code;
      id
    };
    ignore (add_code "entry" code);
    let buf = Buffer.create 0;
    List.iter (fun block => Buffer.add_buffer buf block) (List.rev !blocks);
    Buffer.contents buf
  };
};

let compile_exn compile_env => {
  let rec compile =
    fun
    | Sexp.Sym sym => [Ldv sym]
    | Sexp.Cons _ as s =>
      switch (Sexp.to_list s) {
      | Some [f, ...args] =>
        switch (Value.on_env compile_env f) {
        | Some (Sexp.Pure (Syntax syntax)) => syntax.compile compile args
        | _ => compile_call f args
        }
      | _ => raise (Evaluation_error ("Improper list: " ^ Value.to_string s))
      }
    | s => [Ldc s]
  and compile_call f args =>
    compile f @ List.concat (List.map compile args) @ [App (List.length args)];
  compile
};

let syntax_env () => {
  let compile_def compile =>
    fun
    | [Sexp.Sym sym, x] => compile x @ [Def sym, Ldc Sexp.Nil]
    | _ => raise (Evaluation_error "Syntax error: expected (def sym x)");
  let compile_set compile =>
    fun
    | [Sexp.Sym sym, x] => compile x @ [Set sym, Ldc Sexp.Nil]
    | _ => raise (Evaluation_error "Syntax error: expected (set! sym x)");
  let compile_begin compile => {
    let rec interpret =
      fun
      | [] => [Ldc Sexp.Nil]
      | [x] => compile x
      | [x, ...xs] => compile x @ [Pop] @ interpret xs;
    interpret
  };
  let compile_if compile =>
    fun
    | [c, t, e] => compile c @ [Sel (compile t @ [Leave]) (compile e @ [Leave])]
    | _ => raise (Evaluation_error "Syntax error: expected (if cond then else)");
  let compile_fun compile =>
    fun
    | [pattern, ...body] => [Ldf (Pattern.build pattern) (compile_begin compile body @ [Leave])]
    | [] => raise (Evaluation_error "Syntax error: expected (fun pattern body...)");
  let compile_macro compile =>
    fun
    | [pattern, ...body] => [Ldm (Pattern.build pattern) (compile_begin compile body)]
    | [] => raise (Evaluation_error "Syntax error: expected (macro pattern body...)");
  let compile_builtin compile =>
    fun
    | [Sexp.Sym sym] => [Ldb sym]
    | _ => raise (Evaluation_error "Syntax error: expected (builtin sym)");
  let compile_quote compile =>
    fun
    | [s] => [Ldc s]
    | _ => raise (Evaluation_error "Syntax error: expected (quote expr)");
  let expand bs expand ps => {
    let rec go =
      fun
      | ([b, ...bs], [s, ...ss]) => [
          if b {
            expand s
          } else {
            s
          },
          ...go (bs, ss)
        ]
      | (_, rest) => List.map expand rest;
    go (bs, ps)
  };
  let env = Env.create None;
  List.iter
    (fun (k, expand, compile) => Env.def k (Sexp.Pure (Syntax {expand, compile})) env)
    [
      ("def", expand [false, true], compile_def),
      ("set!", expand [false, true], compile_set),
      ("begin", expand [], compile_begin),
      ("if", expand [true, true, true], compile_if),
      ("fun", expand [false, true], compile_fun),
      ("macro", expand [false, true], compile_macro),
      ("builtin", expand [false], compile_builtin),
      ("quote", expand [false], compile_quote)
    ];
  env
};

let module Exec = {
  type t = state;
  let push state v => state.stack = [v, ...state.stack];
  let pop state =>
    switch state.stack {
    | [hd, ...tl] =>
      state.stack = tl;
      hd
    | [] => raise (Internal_error "Inconsistent stack")
    };
  let enter state env code => {
    switch state.code {
    | [Leave] => () /* tailcall: skip this frame */
    | _ => state.dump = [(state.env, state.code), ...state.dump]
    };
    state.env = env;
    state.code = code
  };
  let leave state =>
    switch state.dump {
    | [(env, code), ...rest] =>
      state.dump = rest;
      state.env = env;
      state.code = code
    | [] => raise (Internal_error "Inconsistent dump")
    };
  let apply state f args =>
    switch f {
    | Sexp.Pure (Fun fenv fpat fbody_code) =>
      enter state (Env.create (Some fenv)) fbody_code;
      Pattern.bind fpat args state.env
    | Sexp.Pure (Builtin builtin) => builtin.run state args
    | _ => raise (Evaluation_error ("Cannot call: " ^ Value.to_string f))
    };
  let capture_cont state => {
    let {stack, env, code, dump} = state;
    let ret v => {
      state.stack = stack;
      state.env = env;
      state.code = code;
      state.dump = dump;
      push state v
    };
    let run state =>
      fun
      | [] => ret Sexp.Nil
      | [x] => ret x
      | _ => raise (Evaluation_error "Multiple values are not implemented");
    Sexp.Pure (Builtin {run: run})
  };
  let inst state =>
    fun
    | Ldc v => push state v
    | Ldv k => push state (Env.get k state.env)
    | Ldf pattern code => push state (Sexp.Pure (Fun state.env pattern code))
    | Ldm pattern code => push state (Sexp.Pure (Macro state.env pattern code))
    | Ldb b =>
      switch (Hashtbl.find_all state.context.builtins b) {
      | [builtin, ..._] => push state (Sexp.Pure (Builtin builtin))
      | _ => raise (Evaluation_error ("Unsupported builtin: " ^ b))
      }
    | Sel a b => {
        let branch_code =
          if (Sexp.test (pop state)) {
            a
          } else {
            b
          };
        enter state (Env.create (Some state.env)) branch_code
      }
    | App argc => {
        let args = ref [];
        for i in 1 to argc {
          args := [pop state, ...!args]
        };
        let f = pop state;
        apply state f !args
      }
    | Leave => leave state
    | Pop => ignore (pop state)
    | Def k => {
        let v = pop state;
        Env.def k v state.env
      }
    | Set k => {
        let v = pop state;
        Env.set k v state.env
      };
  let rec run state =>
    switch state.code {
    | [i, ...rest] =>
      state.code = rest;
      inst state i;
      run state
    | [] => pop state
    };
};

let exec_exn context env code => Exec.run {stack: [], env, code, dump: [], context};

let macroexpand_exn recurse context => {
  let rec expand s =>
    switch (Sexp.to_list s) {
    | Some [m, ...args] =>
      switch (Value.on_env context.toplevel m) {
      | Some (Sexp.Pure (Macro menv mpat mbody_code)) =>
        let env = Env.create (Some menv);
        Pattern.bind mpat args env;
        let s = exec_exn context env mbody_code;
        if recurse {
          expand s
        } else {
          s
        }
      | Some (Sexp.Pure (Syntax syntax)) =>
        if recurse {
          Sexp.of_list [m, ...syntax.expand expand args]
        } else {
          s
        }
      | _ => expand_children s
      }
    | _ => expand_children s
    }
  and expand_children s =>
    if recurse {
      switch s {
      | Sexp.Cons a b => Sexp.Cons (expand a) (expand_children b)
      | s => s
      }
    } else {
      s
    };
  expand
};

let try_context f =>
  try (Ok (f ())) {
  | Env.UndefinedVariable e => Error ("Undefined variable: " ^ e)
  | Internal_error e => Error ("Internal error: " ^ e)
  | Evaluation_error e => Error ("Evaluation error: " ^ e)
  };

let create () => {toplevel: Env.create (Some (syntax_env ())), builtins: Hashtbl.create 0};

let compile context value => try_context (fun _ => compile_exn context.toplevel value);

let macroexpand recurse context value => try_context (
  fun _ => macroexpand_exn recurse context value
);

let eval context s => try_context (
  fun _ => {
    let s = macroexpand_exn true context s;
    let code = compile_exn context.toplevel s;
    exec_exn context context.toplevel code
  }
);

let context state => state.context;

let register_builtin name run context => Hashtbl.add context.builtins name {run: run};
