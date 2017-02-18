exception Internal_error of string
exception Evaluation_error of string

type code = inst list
and inst =
  | Ldc of value
  | Ldv of string
  | Ldf of params * code
  | Ldm of params * code
  | Ldb of string
  | Sel of code * code
  | App of int
  | Leave
  | Pop
  | Def of string
  | Set of string

and params = {
  fixed: string list;
  rest: string option;
}

and value = native Sexp.t
and native =
  | Fun of env * params * code
  | Builtin of builtin
  | Macro of env * params * code
  | Syntax of syntax

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

  let to_string =
    Sexp.to_string @@ function
      | Fun _ -> "<fun>"
      | Builtin _ -> "<builtin>"
      | Macro _ -> "<macro>"
      | Syntax _ -> "<syntax>"

  let is_proc = function
    | Sexp.Pure (Fun _ | Builtin _) -> true
    | _ -> false

  let is_meta = function
    | Sexp.Pure (Macro _ | Syntax _) -> true
    | _ -> false

  let on_env env = function
    | Sexp.Sym sym -> Env.find sym env
    | _ -> None
end


module Params = struct
  type t = params

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
    | Sexp.Cons (Sexp.Sym sym, b) ->
      let { fixed; rest } = build b in
      { fixed = sym :: fixed; rest }
    | s -> raise (Evaluation_error ("Unsupported params: " ^ Value.to_string s))

  let bind params args env =
    let argument_error prefix =
      raise (Evaluation_error ("This function takes " ^ prefix ^ string_of_int (List.length params.fixed) ^ " arguments"))
    in
    let rec bind_fixed = function
      | param :: params, arg :: args -> Env.def param arg env; bind_fixed (params, args)
      | [], args -> args
      | _, [] -> argument_error "at least "
    in
    match params.rest, bind_fixed (params.fixed, args) with
    | Some k, rest_args -> Env.def k (Sexp.of_list rest_args) env
    | None, [] -> ()
    | None, rest_args -> argument_error ""
end

module Code = struct
  type t = code

  let to_string code =
    let block_id = ref 0 in
    let blocks = ref [] in
    let rec inst_to_string = function
      | Ldc c -> "ldc " ^ Value.to_string c
      | Ldv v -> "ldv " ^ v
      | Ldf (params, code) -> "ldf " ^ add_code ("fun " ^ Params.to_string params) code
      | Ldm (params, code) -> "ldm " ^ add_code ("macro " ^ Params.to_string params) code
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


let compile compile_env =
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


let syntax_env =
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
    | params :: body -> [Ldf (Params.build params, compile_begin compile body @ [Leave])]
    | [] -> raise (Evaluation_error "Syntax error: expected (fun params body...)")
  in
  let compile_macro compile = function
    | params :: body -> [Ldm (Params.build params, compile_begin compile body)]
    | [] -> raise (Evaluation_error "Syntax error: expected (macro params body...)")
  in
  let compile_builtin compile = function
    | [Sexp.Sym sym] -> [Ldb sym]
    | _ -> raise (Evaluation_error "Syntax error: expected (builtin sym)")
  in
  let compile_quote compile = function
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
    | Sexp.Pure (Fun (fenv, fparams, fbody_code)) ->
      enter state (Env.create (Some fenv)) fbody_code;
      Params.bind fparams args state.env
    | Sexp.Pure (Builtin builtin) ->
      builtin.run state args
    | _ ->
      raise (Evaluation_error ("Cannot call: " ^ Value.to_string f))

  let capture_cont state =
    let { stack; env; code; dump } = state in
    let ret v =
      state.stack <- stack;
      state.env <- env;
      state.code <- code;
      state.dump <- dump;
      push state v
    in
    let run state = function
      | [] -> ret Sexp.Nil
      | [x] -> ret x
      | _ -> raise (Evaluation_error "Multiple values are not implemented")
    in
    Sexp.Pure (Builtin { run })

  let inst state = function
    | Ldc v ->
      push state v
    | Ldv k ->
      push state (Env.get k state.env)
    | Ldf (params, code) ->
      push state (Sexp.Pure (Fun (state.env, params, code)))
    | Ldm (params, code) ->
      push state (Sexp.Pure (Macro (state.env, params, code)))
    | Ldb b ->
      begin match Hashtbl.find_all state.context.builtins b with
        | builtin :: _ -> push state (Sexp.Pure (Builtin builtin))
        | _ -> raise (Evaluation_error ("Unsupported builtin: " ^ b))
      end
    | Sel (a, b) ->
      let branch_code = if Sexp.to_bool (pop state) then a else b in
      enter state (Env.create (Some state.env)) branch_code
    | App argc ->
      let args = ref [] in
      for i = 1 to argc do args := pop state :: !args done;
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


let create () =
  { toplevel = Env.create (Some syntax_env); builtins = Hashtbl.create 0; }

let exec context env code =
  Exec.run { stack = []; env; code; dump = []; context }

let macroexpand recurse context =
  let rec expand s = match Sexp.to_list s with
    | Some (f :: args) ->
      begin match Value.on_env context.toplevel f with
        | Some (Sexp.Pure (Macro (menv, mparams, mbody_code))) ->
          let env = Env.create (Some menv) in
          Params.bind mparams args env;
          let s = exec context env mbody_code in
          if recurse then expand s else s
        | Some (Sexp.Pure (Syntax syntax)) ->
          if recurse then Sexp.of_list (f :: syntax.expand expand args) else s
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

let eval context s =
  try
    let s = macroexpand true context s in
    (*
    print_endline "Macro expanded:";
    print_endline (Value.to_string s);
    *)
    let code = compile context.toplevel s in
    (*
    print_endline "Compiled VM code:";
    print_endline (Code.to_string code);
    *)
    let result = exec context context.toplevel code in
    (*
    print_endline "Result:";
    print_endline (Value.to_string result);
    *)
    Ok result
  with
  | Env.UndefinedVariable e -> Error ("Undefined variable: " ^ e)
  | Internal_error e -> Error ("Internal error: " ^ e)
  | Evaluation_error e -> Error ("Evaluation error: " ^ e)

let context state =
  state.context

let register_builtin name run context =
  Hashtbl.add context.builtins name { run }
