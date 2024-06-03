exception Internal_error of string
exception Evaluation_error of string

type code = inst list
and inst =
  | Ldc of value
  | Ldv of string
  | Ldf of pattern * code
  | Ldm of pattern * code
  | Ldb of string
  | Sel of code * code
  | App of int
  | Leave
  | Pop
  | Def of string
  | Set of string

and pattern = {
  fixed: string list;
  rest: string option;
}

and value = native Sexp.t
and native =
  | Fun of env * pattern * code
  | Builtin of builtin
  | Macro of env * pattern * code
  | Syntax of syntax
  | Vec of value array

and builtin = {
  run: state -> value list -> unit;
}

and syntax = {
  expand: (value -> value) -> value list -> value list;
  compile: (value -> code) -> value list -> code;
}

and state = {
  mutable stack: value list;
  mutable env: env;
  mutable code: code;
  mutable dump: (env * code) list;
  context: t;
}
and env = value Env.t

and t = {
  toplevel: env;
  builtins: (string, builtin) Hashtbl.t;
}


module Value = struct
  type t = value

  let rec to_string x =
    Sexp.to_string (function
      | Fun _ -> "<fun>"
      | Builtin _ -> "<builtin>"
      | Macro _ -> "<macro>"
      | Syntax _ -> "<syntax>"
      | Vec arr -> to_string (Sexp.of_list (Sexp.Sym "vec" :: Array.to_list arr))
    ) x

  let of_vec arr =
    Sexp.Pure (Vec arr)

  let to_vec = function
    | Sexp.Pure (Vec arr) -> Some arr
    | _ -> None

  let is_proc = function
    | Sexp.Pure (Fun _ | Builtin _) -> true
    | _ -> false

  let is_meta = function
    | Sexp.Pure (Macro _ | Syntax _) -> true
    | _ -> false

  let is_vec = function
    | Sexp.Pure (Vec _) -> true
    | _ -> false

  let on_env env = function
    | Sexp.Sym sym -> Env.find sym env
    | _ -> None
end


module Pattern = struct
  let to_string { fixed; rest } =
    let fixed = List.map (fun sym -> Sexp.Sym sym) fixed in
    let rest = match rest with
      | Some s -> Sexp.Sym s
      | None -> Sexp.Nil
    in
    Value.to_string (List.fold_right (fun a b -> Sexp.Cons (a, b)) fixed rest)

  let rec build = function
    | Sexp.Sym sym -> { fixed = []; rest = Some sym }
    | Sexp.Nil -> { fixed = []; rest = None }
    | Sexp.Cons (a, b) ->
      begin match a with
      | Sexp.Sym sym ->
        let { fixed; rest } = build b in
        { fixed = sym :: fixed; rest }
      | a -> raise (Evaluation_error ("Unsupported pattern: " ^ Value.to_string a))
      end
    | s -> raise (Evaluation_error ("Unsupported pattern: " ^ Value.to_string s))

  let bind pattern args env =
    let argument_error prefix =
      raise (Evaluation_error ("This function takes " ^ prefix ^ string_of_int (List.length pattern.fixed) ^ " arguments"))
    in
    let rec bind_fixed = function
      | p :: pattern, arg :: args -> Env.def p arg env; bind_fixed (pattern, args)
      | [], args -> args
      | _, [] -> argument_error "at least "
    in
    match pattern.rest, bind_fixed (pattern.fixed, args) with
    | Some k, rest_args -> Env.def k (Sexp.of_list rest_args) env
    | None, [] -> ()
    | None, _ -> argument_error ""
end

module Code = struct
  type t = code

  let to_string code =
    let block_id = ref 0 in
    let blocks = ref [] in
    let rec inst_to_string = function
      | Ldc c -> "ldc " ^ Value.to_string c
      | Ldv v -> "ldv " ^ v
      | Ldf (pattern, code) -> "ldf " ^ add_code ("fun " ^ Pattern.to_string pattern) code
      | Ldm (pattern, code) -> "ldm " ^ add_code ("macro " ^ Pattern.to_string pattern) code
      | Ldb s -> "ldb " ^ s
      | Sel (a, b) ->
        let a = add_code "then" a in
        let b = add_code "else" b in
        "sel " ^ a ^ " " ^ b
      | App n -> "app " ^ string_of_int n
      | Leave -> "leave"
      | Pop -> "pop"
      | Def s -> "def " ^ s
      | Set s -> "set " ^ s
    and add_code header code =
      let id = "[" ^ string_of_int !block_id ^ " " ^ header ^ "]" in
      let buf = Buffer.create 0 in
      block_id := succ !block_id;
      blocks := buf :: !blocks;
      Buffer.add_string buf (id ^ "\n");
      List.iter (fun i -> Buffer.add_string buf ("  " ^ inst_to_string i ^ "\n")) code;
      id
    in
    ignore (add_code "entry" code);
    let buf = Buffer.create 0 in
    List.iter (fun block -> Buffer.add_buffer buf block) (List.rev !blocks);
    Buffer.contents buf
end


let compile_exn compile_env =
  let rec compile = function
    | Sexp.Sym sym -> [Ldv sym]
    | Sexp.Cons _ as s ->
      begin match Sexp.to_list s with
        | Some (f :: args) ->
          begin match Value.on_env compile_env f with
            | Some (Sexp.Pure (Syntax syntax)) -> syntax.compile compile args
            | _ -> compile_call f args
          end
        | _ -> raise (Evaluation_error ("Improper list: " ^ Value.to_string s))
      end
    | s -> [Ldc s]
  and compile_call f args =
    compile f @ List.concat (List.map compile args) @ [App (List.length args)]
  in
  compile


let syntax_env () =
  let compile_def compile = function
    | [Sexp.Sym sym; x] -> compile x @ [Def sym; Ldc Sexp.Nil]
    | _ -> raise (Evaluation_error "Syntax error: expected (def sym x)")
  in
  let compile_set compile = function
    | [Sexp.Sym sym; x] -> compile x @ [Set sym; Ldc Sexp.Nil]
    | _ -> raise (Evaluation_error "Syntax error: expected (set! sym x)")
  in
  let compile_begin compile =
    let rec interpret = function
      | [] -> [Ldc Sexp.Nil]
      | [x] -> compile x
      | x :: xs -> compile x @ [Pop] @ interpret xs
    in
    interpret
  in
  let compile_if compile = function
    | [c; t; e] -> compile c @ [Sel (compile t @ [Leave], compile e @ [Leave])]
    | _ -> raise (Evaluation_error "Syntax error: expected (if cond then else)")
  in
  let compile_fun compile = function
    | pattern :: body -> [Ldf (Pattern.build pattern, compile_begin compile body @ [Leave])]
    | [] -> raise (Evaluation_error "Syntax error: expected (fun pattern body...)")
  in
  let compile_macro compile = function
    | pattern :: body -> [Ldm (Pattern.build pattern, compile_begin compile body)]
    | [] -> raise (Evaluation_error "Syntax error: expected (macro pattern body...)")
  in
  let compile_builtin _compile = function
    | [Sexp.Sym sym] -> [Ldb sym]
    | _ -> raise (Evaluation_error "Syntax error: expected (builtin sym)")
  in
  let compile_quote _compile = function
    | [s] -> [Ldc s]
    | _ -> raise (Evaluation_error "Syntax error: expected (quote expr)")
  in
  let expand bs expand ps =
    let rec go = function
      | b :: bs, s :: ss -> (if b then expand s else s) :: go (bs, ss)
      | _, rest -> List.map expand rest
    in go (bs, ps)
  in
  let env = Env.create None in
  List.iter (fun (k, expand, compile) ->
      Env.def k (Sexp.Pure (Syntax { expand; compile })) env
    ) [
    "def", expand [false; true], compile_def;
    "set!", expand [false; true], compile_set;
    "begin", expand [], compile_begin;
    "if", expand [true; true; true], compile_if;
    "fun", expand [false; true], compile_fun;
    "macro", expand [false; true], compile_macro;
    "builtin", expand [false], compile_builtin;
    "quote", expand [false], compile_quote;
  ];
  env


module Exec = struct
  type t = state

  let push state v =
    state.stack <- v :: state.stack

  let pop state =
    match state.stack with
    | hd :: tl -> state.stack <- tl; hd
    | [] -> raise (Internal_error "Inconsistent stack")

  let enter state env code =
    begin match state.code with
      | [Leave] -> () (* tailcall: skip this frame *)
      | _ -> state.dump <- (state.env, state.code) :: state.dump
    end;
    state.env <- env;
    state.code <- code

  let leave state = match state.dump with
    | (env, code) :: rest ->
      state.dump <- rest;
      state.env <- env;
      state.code <- code
    | [] -> raise (Internal_error "Inconsistent dump")

  let apply state f args = match f with
    | Sexp.Pure (Fun (fenv, fpat, fbody_code)) ->
      enter state (Env.create (Some fenv)) fbody_code;
      Pattern.bind fpat args state.env
    | Sexp.Pure (Builtin builtin) ->
      builtin.run state args
    | _ ->
      raise (Evaluation_error ("Cannot call: " ^ Value.to_string f))

  let apply_never state f args =
    state.stack <- [];
    state.code <- [Leave];
    state.dump <- [];
    apply state f args

  let capture_cont state =
    let { stack; env; code; dump; _ } = state in
    let ret state v =
      state.stack <- stack;
      state.env <- env;
      state.code <- code;
      state.dump <- dump;
      push state v
    in
    let run state = function
      | [] -> ret state Sexp.Nil
      | [x] -> ret state x
      | _ -> raise (Evaluation_error "Multiple values are not implemented")
    in
    Sexp.Pure (Builtin { run })

  let inst state = function
    | Ldc v ->
      push state v
    | Ldv k ->
      push state (Env.get k state.env)
    | Ldf (pattern, code) ->
      push state (Sexp.Pure (Fun (state.env, pattern, code)))
    | Ldm (pattern, code) ->
      push state (Sexp.Pure (Macro (state.env, pattern, code)))
    | Ldb b ->
      begin match Hashtbl.find_all state.context.builtins b with
        | builtin :: _ -> push state (Sexp.Pure (Builtin builtin))
        | _ -> raise (Evaluation_error ("Unsupported builtin: " ^ b))
      end
    | Sel (a, b) ->
      let branch_code = if Sexp.test (pop state) then a else b in
      enter state (Env.create (Some state.env)) branch_code
    | App argc ->
      let args = ref [] in
      for _ = 1 to argc do args := pop state :: !args done;
      let f = pop state in
      apply state f !args
    | Leave ->
      leave state
    | Pop ->
      ignore (pop state)
    | Def k ->
      let v = pop state in
      Env.def k v state.env
    | Set k ->
      let v = pop state in
      Env.set k v state.env

  let rec run state =
    match state.code with
    | i :: rest ->
      state.code <- rest;
      inst state i;
      run state
    | [] -> pop state
end


let exec_exn context env code =
  Exec.run { stack = []; env; code; dump = []; context }

let macroexpand_exn recurse context =
  let rec expand s = match Sexp.to_list s with
    | Some (m :: args) ->
      begin match Value.on_env context.toplevel m with
        | Some (Sexp.Pure (Macro (menv, mpat, mbody_code))) ->
          let env = Env.create (Some menv) in
          Pattern.bind mpat args env;
          let s = exec_exn context env mbody_code in
          if recurse then expand s else s
        | Some (Sexp.Pure (Syntax syntax)) ->
          if recurse then Sexp.of_list (m :: syntax.expand expand args) else s
        | _ -> expand_children s
      end
    | _ -> expand_children s
  and expand_children s =
    if recurse then match s with
      | Sexp.Cons (a, b) -> Sexp.Cons (expand a, expand_children b)
      | s -> s
    else s
  in
  expand

let try_context f =
  try
    Ok (f ())
  with
  | Env.UndefinedVariable e -> Error ("Undefined variable: " ^ e)
  | Internal_error e -> Error ("Internal error: " ^ e)
  | Evaluation_error e -> Error ("Evaluation error: " ^ e)

let create () =
  { toplevel = Env.create (Some (syntax_env ())); builtins = Hashtbl.create 0; }

let compile context value =
  try_context (fun _ -> compile_exn context.toplevel value)

let macroexpand recurse context value =
  try_context (fun _ -> macroexpand_exn recurse context value)

let eval context s =
  try_context (fun _ ->
      let s = macroexpand_exn true context s in
      let code = compile_exn context.toplevel s in
      exec_exn context context.toplevel code
    )

let context state =
  state.context

let register_builtin name run context =
  Hashtbl.add context.builtins name { run }
