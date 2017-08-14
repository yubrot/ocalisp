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

let take_port name = enforce name Vm.Value.to_port

let take_vec name = enforce name Vm.Value.to_vec

let take_port_in p =
  match Port.to_in p with
  | None -> evaluation_error "port is not available for reading"
  | Some ch -> ch

let take_port_out p =
  match Port.to_out p with
  | None -> evaluation_error "port is not available for writing"
  | Some ch -> ch

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

let builtin_exit state = function
  | [] -> exit 0
  | [Sexp.Num n] -> exit (int_of_float n)
  | _ -> evaluation_error "exit takes exitcode"

let builtin_error state = function
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

let builtin_test_port = builtin_test "port?" Vm.Value.is_port

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
      | x -> evaluation_error (op ^ " is only defined for strings or numbers")
    )

let builtin_lt = builtin_compare "<" (<) (<)
let builtin_gt = builtin_compare ">" (>) (>)
let builtin_le = builtin_compare "<=" (<=) (<=)
let builtin_ge = builtin_compare ">=" (>=) (>=)

let builtin_callcc state args =
  let f = take_one "call/cc" args in
  let cont = capture_cont state in
  apply state f [cont]

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

let builtin_str state args =
  let bytes = Array.of_list (List.map (take_num "byte") args) in
  let str = String.init (Array.length bytes) @@ fun i ->
    try Char.chr (int_of_float bytes.(i))
    with Invalid_argument _ -> evaluation_error "Each byte of string must be inside the range 0-255"
  in
  push state (Sexp.Str str)

let builtin_str_ref state args =
  let (str, index) = take_two "str-ref" args in
  let str = take_str "string" str in
  let index = take_num "index" index in
  let s =
    try Sexp.Num (float_of_int (Char.code str.[int_of_float index]))
    with Invalid_argument _ -> Sexp.Nil
  in
  push state s

let builtin_str_bytesize state args =
  let str = take_one "str-bytesize" args in
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

let builtin_vec_ref state args =
  let (vec, n) = take_two "vec-ref" args in
  let arr = take_vec "vector" vec in
  let n = int_of_float (take_num "index" n) in
  let s =
    try arr.(n)
    with Invalid_argument _ -> Sexp.Nil
  in
  push state s

let builtin_vec_length state args =
  let vec = take_one "vec-length" args in
  let arr = take_vec "vector" vec in
  let s = Sexp.Num (float_of_int (Array.length arr)) in
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

let sym_eof = Sexp.Sym "eof"

let try_io f =
  try Sexp.Cons (Sexp.Bool true, f ())
  with Sys_error e -> Sexp.Cons (Sexp.Bool false, Sexp.Str e)

let handle_eof f =
  try f ()
  with End_of_file -> sym_eof

let builtin_open state args =
  let (filepath, mode) = take_two "open" args in
  let filepath = take_str "filepath" filepath in
  let mode = take_str "mode" mode in
  push state @@ try_io @@ fun () ->
    match mode with
    | "r" -> Vm.Value.of_port (Port.of_in (open_in_bin filepath))
    | "w" -> Vm.Value.of_port (Port.of_out (open_out_bin filepath))
    | _ -> evaluation_error ("Unsupported mode for open: " ^ mode)

let builtin_close state args =
  let p = take_one "close" args in
  let p = take_port "port" p in
  push state @@ try_io @@ fun () ->
    Port.close p;
    Sexp.Nil

let builtin_stdin state args =
  take_none "stdin" args;
  push state (Vm.Value.of_port (Port.of_in stdin))

let builtin_stdout state args =
  take_none "stdout" args;
  push state (Vm.Value.of_port (Port.of_out stdout))

let builtin_stderr state args =
  take_none "stderr" args;
  push state (Vm.Value.of_port (Port.of_out stderr))

let builtin_read_byte state args =
  let p = take_one "read-byte" args in
  let ch = take_port_in (take_port "port" p) in
  push state @@ try_io @@ fun () ->
    handle_eof @@ fun () ->
      let byte = input_byte ch in
      Sexp.Num (float_of_int byte)

let builtin_read_str state args =
  let (size, p) = take_two "read-str" args in
  let size = int_of_float (take_num "size" size) in
  let ch = take_port_in (take_port "port" p) in
  push state @@ try_io @@ fun () ->
    let bytes = Bytes.create size in
    let bytes_read = input ch bytes 0 size in
    if bytes_read = 0 then
      sym_eof
    else
      Sexp.Str (Bytes.sub_string bytes 0 bytes_read)

let builtin_read_line state args =
  let p = take_one "read-line" args in
  let ch = take_port_in (take_port "port" p) in
  push state @@ try_io @@ fun () ->
    handle_eof @@ fun () ->
      let s = input_line ch in
      Sexp.Str s

let builtin_write_byte state args =
  let (byte, p) = take_two "write-byte" args in
  let byte = int_of_float (take_num "byte" byte) in
  let ch = take_port_out (take_port "port" p) in
  push state @@ try_io @@ fun () ->
    output_byte ch byte;
    Sexp.Num 1.

let builtin_write_str state args =
  let (str, p) = take_two "write-str" args in
  let str = take_str "str" str in
  let ch = take_port_out (take_port "port" p) in
  push state @@ try_io @@ fun () ->
    output_string ch str;
    Sexp.Num (float_of_int (String.length str))

let builtin_write_line state args =
  let (str, p) = take_two "write-line" args in
  let str = take_str "str" str in
  let ch = take_port_out (take_port "port" p) in
  push state @@ try_io @@ fun () ->
    output_string ch str;
    output_char ch '\n';
    flush ch;
    Sexp.Num (float_of_int (String.length str + 1))

let builtin_args_gen env_args state args =
  take_none "args" args;
  push state (Sexp.of_list (List.map (fun s -> Sexp.Str s) env_args))

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
    "port?", builtin_test_port;
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

    "eval", builtin_eval;
    "macroexpand", builtin_macroexpand "macroexpand" true;
    "macroexpand-1", builtin_macroexpand "macroexpand-1" false;

    "str", builtin_str;
    "str-ref", builtin_str_ref;
    "str-bytesize", builtin_str_bytesize;
    "str-concat", builtin_str_concat;
    "substr", builtin_substr;
    "sym->str", builtin_sym_to_str;
    "num->str", builtin_num_to_str;
    "str->num", builtin_str_to_num;

    "vec", builtin_vec;
    "vec-make", builtin_vec_make;
    "vec-ref", builtin_vec_ref;
    "vec-length", builtin_vec_length;
    "vec-set!", builtin_vec_set;
    "vec-copy!", builtin_vec_copy;

    "open", builtin_open;
    "close", builtin_close;

    "stdin", builtin_stdin;
    "stdout", builtin_stdout;
    "stderr", builtin_stderr;

    "read-byte", builtin_read_byte;
    "read-str", builtin_read_str;
    "read-line", builtin_read_line;

    "write-byte", builtin_write_byte;
    "write-str", builtin_write_str;
    "write-line", builtin_write_line;

    "args", builtin_args_gen args;
  ]
