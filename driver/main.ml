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
  | Error e -> prerr_endline (file ^ ": " ^ e); exit 1

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

let init ctx boot args =
  Builtins.register args ctx;
  if boot then
    let lexbuf = Lexing.from_string Bootcode.value in
    match exec ctx lexbuf with
    | Ok _ -> ()
    | Error e -> prerr_endline ("init: " ^ e); exit 1

let () =
  let ctx = Vm.create () in
  match Array.to_list Sys.argv with
  | [] | [_] ->
      init ctx true [];
      repl ctx
  | _ :: "-test" :: tests ->
      init ctx false [];
      List.iter (Testrunner.run ctx) tests
  | _ :: ls ->
      let files = ref [] in
      let args = ref [] in
      let args_started = ref false in
      List.iter (fun s ->
        if !args_started then
          args := s :: !args
        else if s = "--" then
          args_started := true
        else
          files := s :: !files
      ) ls;
      init ctx true (List.rev !args);
      List.iter (exec_file ctx) (List.rev !files)
