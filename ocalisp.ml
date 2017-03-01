let exec ctx lexbuf =
  match Lexer.parse_program lexbuf with
  | Ok program ->
    let rec exec_all = function
      | [] -> Ok ()
      | s :: rest -> match Vm.eval ctx s with
        | Ok _ -> exec_all rest
        | Error e -> Error e
    in
    exec_all program
  | Error e -> Error e

let exec_file ctx file =
  match exec ctx (Lexing.from_channel (open_in file)) with
  | Ok _ -> ()
  | Error e -> prerr_endline e

let repl ctx =
  prerr_endline "[ocalisp REPL]";
  let lexbuf = Lexing.from_channel stdin in
  while true do
    prerr_string "> ";
    flush stderr;
    match Lexer.parse_line lexbuf with
    | Ok s ->
      begin match Vm.eval ctx s with
        | Ok v -> print_endline (Vm.Value.to_string v)
        | Error e -> prerr_endline e
      end
    | Error e -> prerr_endline e
  done

let boot ctx =
  Builtins.register ctx;
  let lexbuf = Lexing.from_channel (open_in "lispboot/boot.lisp") in
  match exec ctx lexbuf with
  | Ok _ -> ()
  | Error e -> prerr_endline e; exit 1

let () =
  let ctx = Vm.create () in
  match Sys.argv with
  | [| _; "-test"; test |] -> Builtins.register ctx; Testrunner.run ctx test
  | [| _; file |] -> boot ctx; exec_file ctx file
  | _ -> boot ctx; repl ctx
