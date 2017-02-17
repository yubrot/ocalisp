let parse p buf = try
    Ok (p Lexer.read buf)
  with
  | _ -> Error "Parse error: TODO provide error location"

let exec_file state src =
  let buf = Lexing.from_channel (open_in src) in
  match parse Parser.program buf with
  | Ok program -> List.iter (fun s ->
      match Vm.eval state s with
      | Ok _ -> ()
      | Error e -> print_endline e; exit 1
    ) program
  | Error e -> print_endline e

let repl state =
  print_endline "[ocalisp REPL]";
  let buf = Lexing.from_channel stdin in
  while true do
    print_string "> ";
    flush stdout;
    match parse Parser.s buf with
    | Ok s ->
      begin match Vm.eval state s with
        | Ok v -> print_endline (Vm.Value.to_string v)
        | Error e -> print_endline e
      end
    | Error e -> print_endline e
  done

let () =
  let state = Vm.create () in
  Builtins.register state;
  exec_file state "boot.lisp";
  match Sys.argv with
  | [| _; src |] -> exec_file state src
  | _ -> repl state
