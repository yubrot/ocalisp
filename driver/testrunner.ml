open Ocalisp

type command =
  | ParseSuccess of (string * string)
  | ParseFailure of string
  | CompileSuccess of (string * string)
  | CompileFailure of string
  | EvalSuccess of (string * string)
  | EvalFailure of string
  | EvalAll of string

type testcase = {
  header: string;
  command: command;
}

let read_lines ch len =
  let buf = Buffer.create 0 in
  for i = 1 to int_of_string len do
    if i <> 1 then Buffer.add_string buf "\n" else ();
    Buffer.add_string buf (input_line ch)
  done;
  Buffer.contents buf

let read_lines2 ch a b =
  let a = read_lines ch a in
  let b = read_lines ch b in
  (a, b)

let parse_testcases file =
  let ch = open_in file in
  let testcases = ref [] in
  try
    while
      match
        try Some (input_line ch)
        with End_of_file -> None
      with
      | None -> false
      | Some header ->
        let command = input_line ch in
        let command = match String.split_on_char ' ' command with
          | ["PARSE_SUCCESS"; input; result] -> ParseSuccess (read_lines2 ch input result)
          | ["PARSE_FAILURE"; input] -> ParseFailure (read_lines ch input)
          | ["COMPILE_SUCCESS"; input; result] -> CompileSuccess (read_lines2 ch input result)
          | ["COMPILE_FAILURE"; input] -> CompileFailure (read_lines ch input)
          | ["EVAL_SUCCESS"; input; result] -> EvalSuccess (read_lines2 ch input result)
          | ["EVAL_FAILURE"; input] -> EvalFailure (read_lines ch input)
          | ["EVAL_ALL"; input] -> EvalAll (read_lines ch input)
          | _ -> failwith ("Unknown test command: " ^ command)
        in
        testcases := { header; command } :: !testcases; true
    do () done;
    close_in ch;
    List.rev !testcases
  with e -> close_in ch; raise e

exception CommandFailed of string

let fail msg = raise (CommandFailed msg)

let fail_if_error = function
  | Ok v -> v
  | Error e -> fail e

let fail_if_differ i o =
  if String.trim i = String.trim o then
    ()
  else
    fail i

let parse_or_fail i =
  fail_if_error (Lexer.parse_line (Lexing.from_string i))

let run_command ctx = function
  | ParseSuccess (i, o) ->
    begin match Lexer.parse_line (Lexing.from_string i) with
      | Error e -> fail e
      | Ok s -> fail_if_differ (Vm.Value.to_string s) o
    end
  | ParseFailure i ->
    begin match Lexer.parse_line (Lexing.from_string i) with
      | Error _ -> ()
      | Ok s -> fail (Vm.Value.to_string s)
    end
  | CompileSuccess (i, o) ->
    begin match Vm.compile ctx (parse_or_fail i) with
      | Error e -> fail e
      | Ok s -> fail_if_differ (Vm.Code.to_string s) o
    end
  | CompileFailure i ->
    begin match Vm.compile ctx (parse_or_fail i) with
      | Error _ -> ()
      | Ok s -> fail (Vm.Code.to_string s)
    end
  | EvalSuccess (i, o) ->
    begin match Vm.eval ctx (parse_or_fail i) with
      | Error e -> fail e
      | Ok s -> fail_if_differ (Vm.Value.to_string s) o
    end
  | EvalFailure i ->
    begin match Vm.eval ctx (parse_or_fail i) with
      | Error _ -> ()
      | Ok s -> fail (Vm.Value.to_string s)
    end
  | EvalAll i ->
    begin match Lexer.parse_program (Lexing.from_string i) with
      | Error e -> fail e
      | Ok program ->
        List.iter (fun line ->
          ignore (fail_if_error (Vm.eval ctx line))
        ) program
    end

let run_testcase ctx { header; command } =
  try run_command ctx command
  with CommandFailed e -> prerr_endline ("Test failed at " ^ header ^ ": " ^ e)

let run ctx test =
  List.iter (run_testcase ctx) (parse_testcases test)
