open Vm.Exec

let evaluation_error msg =
  raise (Vm.Evaluation_error msg)

let type_error msg arg =
  evaluation_error (msg ^ ": " ^ Vm.Value.to_string arg)

let extract_numbers op =
  List.map @@ function
    | Sexp.Num n -> n
    | s -> type_error ("Operator " ^ op ^ " takes number arguments") s

let extract_strings op =
  List.map @@ function
    | Sexp.Str s -> s
    | s -> type_error ("Operator " ^ op ^ " takes string arguments") s

let builtin_cons state = function
  | [a; b] -> push state (Sexp.Cons (a, b))
  | _ -> evaluation_error "Builtin function cons takes 2 arguments"

let builtin_exit state = function
  | [Sexp.Num n] -> exit (int_of_float n)
  | _ -> evaluation_error "Builtin function exit takes a number argument"

let builtin_error state = function
  | [Sexp.Str s] -> evaluation_error s
  | _ -> evaluation_error "Builtin function error takes a string argument"

let builtin_gensym () =
  let id = ref 0 in
  fun state args -> match args with
    | [] -> id := succ !id; push state (Sexp.Sym ("#sym." ^ string_of_int !id))
    | _ -> evaluation_error "Builtin function gensym takes no arguments"

let builtin_car state = function
  | [Sexp.Cons (a, _)] -> push state a
  | [s] -> type_error "Not a cons" s
  | _ -> evaluation_error "Builtin function car takes 1 argument"

let builtin_cdr state = function
  | [Sexp.Cons (_, b)] -> push state b
  | [x] -> type_error "Not a cons" x
  | _ -> evaluation_error "Builtin function car takes 1 argument"

let builtin_apply state = function
  | [f; args] ->
    begin match Sexp.to_list args with
      | Some args -> apply state f args
      | None -> type_error "Improper list passed as apply arguments" args
    end
  | _ -> evaluation_error "Builtin function apply takes 2 arguments"

let builtin_test name test state = function
  | [x] -> push state (Sexp.of_bool (test x))
  | _ -> evaluation_error ("Builtin function " ^ name ^ " takes 1 argument")

let builtin_test_num = builtin_test "num?" @@ function
  | Sexp.Num _ -> true
  | _ -> false

let builtin_test_sym = builtin_test "sym?" @@ function
  | Sexp.Sym _ -> true
  | _ -> false

let builtin_test_str = builtin_test "str?" @@ function
  | Sexp.Str _ -> true
  | _ -> false

let builtin_test_cons = builtin_test "cons?" @@ function
  | Sexp.Cons _ -> true
  | _ -> false

let builtin_test_nil = builtin_test "nil?" @@ function
  | Sexp.Nil -> true
  | _ -> false

let builtin_test_bool = builtin_test "bool?" @@ function
  | Sexp.True | Sexp.False -> true
  | _ -> false

let builtin_test_proc = builtin_test "proc?" Vm.Value.is_proc

let builtin_test_meta = builtin_test "meta?" Vm.Value.is_meta

let builtin_arithmetic op zero one cat state args =
  let nums = extract_numbers op args in
  let result = match nums with
  | [] ->
    begin match zero with
      | None -> evaluation_error ("Operator " ^ op ^ " takes at least one argument")
      | Some n -> n
    end
  | [n] -> one n
  | n :: ns -> List.fold_left cat n ns
  in
  push state (Sexp.Num result)

let builtin_add = builtin_arithmetic "+" (Some 0.) (fun n -> n) (+.)
let builtin_sub = builtin_arithmetic "-" None (fun n -> -. n) (-.)
let builtin_mul = builtin_arithmetic "*" (Some 1.) (fun n -> n) ( *.)
let builtin_div = builtin_arithmetic "/" None (fun n -> 1. /. n) (/.)
let builtin_mod = builtin_arithmetic "%" None (fun n -> n) mod_float

let builtin_concat state args =
  let strs = extract_strings "concat" args in
  push state (Sexp.Str (String.concat "" strs))

let builtin_length state = function
  | [Sexp.Str s] -> push state (Sexp.Num (float_of_int (String.length s)))
  | _ -> evaluation_error "Builtin function error takes a string argument"

let builtin_eq state args =
  let rec equal a b = Sexp.(match a, b with
      | Num a, Num b -> a = b
      | Sym a, Sym b -> a = b
      | Str a, Str b -> a = b
      | Cons (a, a'), Cons (b, b') -> equal a b && equal a' b'
      | Nil, Nil | True, True | False, False -> true
      | _, _ -> false
    )
  in
  match args with
  | [] -> push state Sexp.True
  | x :: xs -> push state (Sexp.of_bool (List.for_all (equal x) xs))

let builtin_compare op num_compare str_compare state args =
  let rec test compare x xs = match xs with
    | [] -> true
    | y :: xs -> compare x y && test compare y xs
  in
  match args with
  | [] -> push state Sexp.True
  | x :: xs -> Sexp.(match x with
      | Num x ->
        let xs = extract_numbers op xs in
        push state (Sexp.of_bool (test num_compare x xs))
      | Str x ->
        let xs = extract_strings op xs in
        push state (Sexp.of_bool (test str_compare x xs))
      | x -> type_error ("Operator " ^ op ^ " is only defined for strings and numbers") x
    )

let builtin_lt = builtin_compare "<" (<) (<)
let builtin_gt = builtin_compare ">" (>) (>)
let builtin_le = builtin_compare "<=" (<=) (<=)
let builtin_ge = builtin_compare ">=" (>=) (>=)

let builtin_callcc state = function
  | [f] ->
    let cont = capture_cont state in
    apply state f [cont]
  | _ -> evaluation_error ("Builtin function call/cc takes 1 argument")

let builtin_eval state = function
  | [s] ->
    begin match Vm.eval (Vm.context state) s with
      | Ok v -> push state v
      | Error e -> evaluation_error ("on eval: " ^ e)
    end
  | _ -> evaluation_error ("Builtin function eval takes 1 argument")

let builtin_macroexpand name recurse state = function
  | [s] -> push state (Vm.macroexpand recurse (Vm.context state) s)
  | _ -> evaluation_error ("Builtin function " ^ name ^ " takes 1 argument")

let builtin_print state args =
  List.iter (function
      | Sexp.Str s -> print_string s
      | s -> type_error "Cannot print non-string argument" s
    ) args;
  push state Sexp.Nil

let builtin_newline state = function
  | [] -> print_newline (); push state Sexp.Nil
  | _ -> evaluation_error "Builtin function car takes no arguments"

let builtin_show state = function
  | [x] -> push state (Sexp.Str (Vm.Value.to_string x))
  | _ -> evaluation_error "Builtin function show takes one argument"

let register context =
  List.iter (fun (name, f) ->
      Vm.register_builtin name f context
    ) [
    "cons", builtin_cons;

    "exit", builtin_exit;
    "error", builtin_error;

    "gensym", builtin_gensym ();

    "car", builtin_car;
    "cdr", builtin_cdr;

    "apply", builtin_apply;

    "num?", builtin_test_num;
    "sym?", builtin_test_sym;
    "str?", builtin_test_str;
    "cons?", builtin_test_cons;
    "nil?", builtin_test_nil;
    "bool?", builtin_test_bool;
    "proc?", builtin_test_proc;
    "meta?", builtin_test_meta;

    "+", builtin_add;
    "-", builtin_sub;
    "*", builtin_mul;
    "/", builtin_div;
    "%", builtin_mod;

    "concat", builtin_concat;
    "length", builtin_length;

    "=", builtin_eq;
    "<", builtin_lt;
    ">", builtin_gt;
    "<=", builtin_le;
    ">=", builtin_ge;

    "call/cc", builtin_callcc;

    "eval", builtin_eval;
    "macroexpand", builtin_macroexpand "macroexpand" true;
    "macroexpand-1", builtin_macroexpand "macroexpand-1" false;

    "print", builtin_print;
    "newline", builtin_newline;

    "show", builtin_show;
  ]
