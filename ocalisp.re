open Compat;

let exec ctx lexbuf =>
  switch (Lexer.parse_program lexbuf) {
  | Ok program =>
    let rec exec_all = (
      fun
      | [] => Ok ()
      | [s, ...rest] =>
        switch (Vm.eval ctx s) {
        | Ok _ => exec_all rest
        | Error e => Error e
        }
    );
    exec_all program
  | Error e => Error e
  };

let exec_file ctx file =>
  switch (exec ctx (Lexing.from_channel (open_in file))) {
  | Ok _ => ()
  | Error e => prerr_endline e
  };

let repl ctx => {
  prerr_endline "[ocalisp REPL]";
  let lexbuf = Lexing.from_channel stdin;
  while true {
    prerr_string "> ";
    flush stderr;
    switch (Lexer.parse_line lexbuf) {
    | Ok s =>
      switch (Vm.eval ctx s) {
      | Ok v => print_endline (Vm.Value.to_string v)
      | Error e => prerr_endline e
      }
    | Error e => prerr_endline e
    }
  }
};

let boot ctx => {
  Builtins.register ctx;
  let lexbuf = Lexing.from_channel (open_in "lispboot/boot.lisp");
  switch (exec ctx lexbuf) {
  | Ok _ => ()
  | Error e =>
    prerr_endline e;
    exit 1
  }
};

let () = {
  let ctx = Vm.create ();
  switch Sys.argv {
  | [|_, "-test", test|] =>
    Builtins.register ctx;
    Testrunner.run ctx test
  | [|_, file|] =>
    boot ctx;
    exec_file ctx file
  | _ =>
    boot ctx;
    repl ctx
  }
};
