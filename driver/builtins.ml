open Ocalisp
open Vm.Exec

let evaluation_error msg =
  raise (Vm.Evaluation_error msg)

let enforce name extract s =
  match extract s with
  | Some v -> v
  | None -> evaluation_error ("Expected " ^ name ^ " but got " ^ Vm.Value.to_string s)

let take_num name = enforce name @@ function
  | Sexp.Num n -> Some n
  | _ -> None

let take_sym name = enforce name @@ function
  | Sexp.Sym s -> Some s
  | _ -> None

let take_str name = enforce name @@ function
  | Sexp.Str s -> Some s
  | _ -> None

let take_cons name = enforce name @@ function
  | Sexp.Cons (a, b) -> Some (a, b)
  | _ -> None

let take_list name = enforce name Sexp.to_list

let take_vec name = enforce name Vm.Value.to_vec

let take_none name = function
  | [] -> ()
  | _ -> evaluation_error (name ^ " takes no arguments")

let take_one name = function
  | [a] -> a
  | _ -> evaluation_error (name ^ " takes one argument")

let take_two name = function
  | [a; b] -> (a, b)
  | _ -> evaluation_error (name ^ " takes two arguments")

let take_three name = function
  | [a; b; c] -> (a, b, c)
  | _ -> evaluation_error (name ^ " takes three arguments")

let take_five name = function
  | [a; b; c; d; e] -> (a, b, c, d, e)
  | _ -> evaluation_error (name ^ " takes five arguments")

let builtin_cons state args =
  let (a, b) = take_two "cons" args in
  push state (Sexp.Cons (a, b))

let builtin_exit _state = function
  | [] -> exit 0
  | [Sexp.Num n] -> exit (int_of_float n)
  | _ -> evaluation_error "exit takes exitcode"

let builtin_error _state = function
  | [] -> evaluation_error "error called"
  | [s] -> evaluation_error (take_str "error message" s)
  | _ -> evaluation_error "error takes an error message"

let builtin_gensym_gen () =
  let id = ref 0 in
  fun state args ->
    take_none "gensym" args;
    id := succ !id;
    push state (Sexp.Sym ("#sym." ^ string_of_int !id))

let builtin_car state args =
  let a = take_one "car" args in
  let (a, _) = take_cons "cons" a in
  push state a

let builtin_cdr state args =
  let a = take_one "cdr" args in
  let (_, a) = take_cons "cons" a in
  push state a

let builtin_apply state args =
  let (f, args) = take_two "apply" args in
  let args = take_list "argument list" args in
  apply state f args

let builtin_test name test state args =
  let a = take_one name args in
  push state (Sexp.Bool (test a))

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
  | Sexp.Bool _ -> true
  | _ -> false

let builtin_test_proc = builtin_test "proc?" Vm.Value.is_proc

let builtin_test_meta = builtin_test "meta?" Vm.Value.is_meta

let builtin_test_vec = builtin_test "vec?" Vm.Value.is_vec

let builtin_arithmetic op zero one cat state args =
  let nums = List.map (take_num "number") args in
  let result = match nums with
  | [] ->
    begin match zero with
    | None -> evaluation_error (op ^ " takes at least one argument")
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

let builtin_eq state args =
  let rec equal a b = Sexp.(match a, b with
      | Num a, Num b -> a = b
      | Sym a, Sym b -> a = b
      | Str a, Str b -> a = b
      | Cons (a, a'), Cons (b, b') -> equal a b && equal a' b'
      | Nil, Nil -> true
      | Bool a, Bool b -> a = b
      | _, _ -> false
    )
  in
  match args with
  | [] -> push state (Sexp.Bool true)
  | x :: xs -> push state (Sexp.Bool (List.for_all (equal x) xs))

let builtin_compare op num_compare str_compare state args =
  let rec cmp compare x xs = match xs with
    | [] -> true
    | y :: xs -> compare x y && cmp compare y xs
  in
  match args with
  | [] -> push state (Sexp.Bool true)
  | x :: xs -> Sexp.(match x with
      | Num x ->
        let xs = List.map (take_num "number") xs in
        push state (Sexp.Bool (cmp num_compare x xs))
      | Str x ->
        let xs = List.map (take_str "string") xs in
        push state (Sexp.Bool (cmp str_compare x xs))
      | _ -> evaluation_error (op ^ " is only defined for strings or numbers")
    )

let builtin_lt = builtin_compare "<" (<) (<)
let builtin_gt = builtin_compare ">" (>) (>)
let builtin_le = builtin_compare "<=" (<=) (<=)
let builtin_ge = builtin_compare ">=" (>=) (>=)

let builtin_callcc state args =
  let f = take_one "call/cc" args in
  let cont = capture_cont state in
  apply state f [cont]

let builtin_never state args =
  match args with
  | [] -> evaluation_error "never takes at least one argument"
  | f :: args -> apply_never state f args

let builtin_str state args =
  let bytes = Array.of_list (List.map (take_num "byte") args) in
  let str = String.init (Array.length bytes) @@ fun i ->
    try Char.chr (int_of_float bytes.(i))
    with Invalid_argument _ -> evaluation_error "Each byte of string must be inside the range 0-255"
  in
  push state (Sexp.Str str)

let builtin_str_char_at state args =
  let (str, index) = take_two "str-char-at" args in
  let str = take_str "string" str in
  let index = take_num "index" index in
  let s =
    try Sexp.Num (float_of_int (Char.code str.[int_of_float index]))
    with Invalid_argument _ -> Sexp.Nil
  in
  push state s

let builtin_str_length state args =
  let str = take_one "str-length" args in
  let str = take_str "string" str in
  push state (Sexp.Num (float_of_int (String.length str)))

let builtin_str_concat state args =
  let strs = List.map (take_str "string") args in
  push state (Sexp.Str (String.concat "" strs))

let builtin_substr state args =
  let (s, i, l) = take_three "substr" args in
  let s = take_str "string" s in
  let i = take_num "index" i in
  let l = take_num "size" l in
  let s =
    try String.sub s (int_of_float i) (int_of_float l)
    with Invalid_argument _ -> evaluation_error "Index out of range"
  in
  push state (Sexp.Str s)

let builtin_sym_to_str state args =
  let x = take_one "sym->str" args in
  let s = take_sym "symbol" x in
  push state (Sexp.Str s)

let builtin_num_to_str state args =
  let x = take_one "num->str" args in
  let _ = take_num "number" x in
  push state (Sexp.Str (Vm.Value.to_string x))

let builtin_str_to_num state args =
  let x = take_one "str->num" args in
  let s = take_str "string" x in
  let s =
    try Sexp.Num (float_of_string s)
    with Failure _ -> Sexp.Nil
  in
  push state s

let builtin_vec state args =
  let s = Vm.Value.of_vec (Array.of_list args) in
  push state s

let builtin_vec_make state args =
  let (length, init) = take_two "vec-make" args in
  let length = int_of_float (take_num "length" length) in
  let s = Vm.Value.of_vec (Array.make length init) in
  push state s

let builtin_vec_length state args =
  let vec = take_one "vec-length" args in
  let arr = take_vec "vector" vec in
  let s = Sexp.Num (float_of_int (Array.length arr)) in
  push state s

let builtin_vec_get state args =
  let (vec, n) = take_two "vec-get" args in
  let arr = take_vec "vector" vec in
  let n = int_of_float (take_num "index" n) in
  let s =
    try arr.(n)
    with Invalid_argument _ -> Sexp.Nil
  in
  push state s

let builtin_vec_set state args =
  let (vec, n, item) = take_three "vec-set!" args in
  let arr = take_vec "vector" vec in
  let n = int_of_float (take_num "index" n) in
  begin
    try arr.(n) <- item
    with Invalid_argument _ -> evaluation_error "Index out of range"
  end;
  push state Sexp.Nil

let builtin_vec_copy state args =
  let (dest, dest_start, src, src_start, length) = take_five "vec-copy!" args in
  let dest = take_vec "destination vector" dest in
  let dest_start = int_of_float (take_num "destination index" dest_start) in
  let src = take_vec "source vector" src in
  let src_start = int_of_float (take_num "source index" src_start) in
  let length = int_of_float (take_num "length" length) in
  begin
    try Array.blit src src_start dest dest_start length
    with Invalid_argument _ -> evaluation_error "Index out of range"
  end;
  push state Sexp.Nil

let try_io f =
  try Sexp.Cons (Sexp.Bool true, f ())
  with Sys_error e -> Sexp.Cons (Sexp.Bool false, Sexp.Str e)

let builtin_read_file_text state args =
  let filepath = take_one "read-file-text" args in
  let filepath = take_str "filepath" filepath in
  push state @@ try_io @@ fun () ->
    let chan = open_in_bin filepath in
    let len = in_channel_length chan in
    let buffer = Bytes.create len in
    really_input chan buffer 0 len;
    close_in chan;
    Sexp.Str(Bytes.to_string buffer)

let builtin_write_file_text state args =
  let (filepath, text) = take_two "write-file-text" args in
  let filepath = take_str "filepath" filepath in
  let text = take_str "text" text in
  push state @@ try_io @@ fun () ->
    let chan = open_out_bin filepath in
    output_string chan text;
    close_out chan;
    Sexp.Nil

let builtin_read_console_line state args =
  take_none "builtin_read_console_line " args;
  push state @@ try_io @@ fun () ->
    try Sexp.Str (read_line ())
    with End_of_file -> Sexp.Nil

let builtin_write_console state args =
  let text = take_one "write-console" args in
  let text = take_str "filepath" text in
  push state @@ try_io @@ fun () ->
    print_string text;
    flush stdout;
    Sexp.Nil

let builtin_args_gen env_args state args =
  take_none "args" args;
  push state (Sexp.of_list (List.map (fun s -> Sexp.Str s) env_args))

let builtin_eval state args =
  let s = take_one "eval" args in
  let result =
    match Vm.eval (Vm.context state) s with
    | Ok v -> Sexp.Cons (Sexp.Bool true, v)
    | Error e -> Sexp.Cons (Sexp.Bool false, Sexp.Str e)
  in
  push state result

let builtin_macroexpand name recurse state args =
  let s = take_one name args in
  let result =
    match Vm.macroexpand recurse (Vm.context state) s with
    | Ok v -> Sexp.Cons (Sexp.Bool true, v)
    | Error e -> Sexp.Cons (Sexp.Bool false, Sexp.Str e)
  in
  push state result

let register args context =
  List.iter (fun (name, f) ->
      Vm.register_builtin name f context
    ) [
    "cons", builtin_cons;

    "exit", builtin_exit;
    "error", builtin_error;

    "gensym", builtin_gensym_gen ();

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
    "vec?", builtin_test_vec;

    "+", builtin_add;
    "-", builtin_sub;
    "*", builtin_mul;
    "/", builtin_div;
    "%", builtin_mod;

    "=", builtin_eq;
    "<", builtin_lt;
    ">", builtin_gt;
    "<=", builtin_le;
    ">=", builtin_ge;

    "call/cc", builtin_callcc;
    "never", builtin_never;

    "str", builtin_str;
    "str-char-at", builtin_str_char_at;
    "str-length", builtin_str_length;
    "str-concat", builtin_str_concat;
    "substr", builtin_substr;
    "sym->str", builtin_sym_to_str;
    "num->str", builtin_num_to_str;
    "str->num", builtin_str_to_num;

    "vec", builtin_vec;
    "vec-make", builtin_vec_make;
    "vec-length", builtin_vec_length;
    "vec-get", builtin_vec_get;
    "vec-set!", builtin_vec_set;
    "vec-copy!", builtin_vec_copy;

    "read-file-text", builtin_read_file_text;
    "write-file-text", builtin_write_file_text;
    "read-console-line", builtin_read_console_line;
    "write-console", builtin_write_console;

    "args", builtin_args_gen args;
    "eval", builtin_eval;
    "macroexpand", builtin_macroexpand "macroexpand" true;
    "macroexpand-1", builtin_macroexpand "macroexpand-1" false;
  ]
