open Compat;

type command =
  | ParseSuccess (string, string)
  | ParseFailure string
  | CompileSuccess (string, string)
  | CompileFailure string
  | EvalSuccess (string, string)
  | EvalFailure string
  | EvalAll string;

type testcase = {header: string, command: command};

let read_lines ch len => {
  let buf = Buffer.create 0;
  for i in 1 to (int_of_string len) {
    if (i != 1) {
      Buffer.add_string buf "\n"
    } else {
      ()
    };
    Buffer.add_string buf (input_line ch)
  };
  Buffer.contents buf
};

let read_lines2 ch a b => {
  let a = read_lines ch a;
  let b = read_lines ch b;
  (a, b)
};

let parse_testcases file => {
  let ch = open_in file;
  let testcases = ref [];
  try {
    while (
      switch (
        try (Some (input_line ch)) {
        | End_of_file => None
        }
      ) {
      | None => false
      | Some header =>
        let command = input_line ch;
        let command =
          switch (String.split_on_char ' ' command) {
          | ["PARSE_SUCCESS", input, result] => ParseSuccess (read_lines2 ch input result)
          | ["PARSE_FAILURE", input] => ParseFailure (read_lines ch input)
          | ["COMPILE_SUCCESS", input, result] => CompileSuccess (read_lines2 ch input result)
          | ["COMPILE_FAILURE", input] => CompileFailure (read_lines ch input)
          | ["EVAL_SUCCESS", input, result] => EvalSuccess (read_lines2 ch input result)
          | ["EVAL_FAILURE", input] => EvalFailure (read_lines ch input)
          | ["EVAL_ALL", input] => EvalAll (read_lines ch input)
          | _ => failwith ("Unknown test command: " ^ command)
          };
        testcases := [{header, command}, ...!testcases];
        true
      }
    ) {
      ()
    };
    close_in ch;
    List.rev !testcases
  } {
  | e =>
    close_in ch;
    raise e
  }
};

exception CommandFailed string;

let fail msg => raise (CommandFailed msg);

let fail_if_error =
  fun
  | Ok v => v
  | Error e => fail e;

let fail_if_differ i o =>
  if (String.trim i == String.trim o) {
    ()
  } else {
    fail i
  };

let parse_or_fail i => fail_if_error (Lexer.parse_line (Lexing.from_string i));

let run_command ctx =>
  fun
  | ParseSuccess (i, o) =>
    switch (Lexer.parse_line (Lexing.from_string i)) {
    | Error e => fail e
    | Ok s => fail_if_differ (Vm.Value.to_string s) o
    }
  | ParseFailure i =>
    switch (Lexer.parse_line (Lexing.from_string i)) {
    | Error _ => ()
    | Ok s => fail (Vm.Value.to_string s)
    }
  | CompileSuccess (i, o) =>
    switch (Vm.compile ctx (parse_or_fail i)) {
    | Error e => fail e
    | Ok s => fail_if_differ (Vm.Code.to_string s) o
    }
  | CompileFailure i =>
    switch (Vm.compile ctx (parse_or_fail i)) {
    | Error _ => ()
    | Ok s => fail (Vm.Code.to_string s)
    }
  | EvalSuccess (i, o) =>
    switch (Vm.eval ctx (parse_or_fail i)) {
    | Error e => fail e
    | Ok s => fail_if_differ (Vm.Value.to_string s) o
    }
  | EvalFailure i =>
    switch (Vm.eval ctx (parse_or_fail i)) {
    | Error _ => ()
    | Ok s => fail (Vm.Value.to_string s)
    }
  | EvalAll i =>
    switch (Lexer.parse_program (Lexing.from_string i)) {
    | Error e => fail e
    | Ok program => List.iter (fun line => ignore (fail_if_error (Vm.eval ctx line))) program
    };

let run_testcase ctx {header, command} =>
  try (run_command ctx command) {
  | CommandFailed e => prerr_endline ("Test failed at " ^ header ^ ": " ^ e)
  };

let run ctx test => List.iter (run_testcase ctx) (parse_testcases test);
