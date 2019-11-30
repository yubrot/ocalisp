{
  open Parser
  open Lexing
  exception SyntaxError of string
}

let letter = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let special = ['!' '$' '%' '&' '*' '+' '-' '/' ':' '<' '=' '>' '?' '@' '^' '_' '~']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+

let num = ['-' '+']? digit+ frac? exp?
let sym = (letter | special) (letter | digit | special)*

rule read = parse
  | [' ' '\t' '\n']+ { read lexbuf }
  | ';' [^ '\n']* { read lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '.' { DOT }
  | "#t" { TRUE }
  | "#f" { FALSE }
  | '\'' { QUOTE }
  | '`' { QUASIQUOTE }
  | ',' { UNQUOTE }
  | ",@" { UNQUOTE_SPLICING }
  | num { NUM (float_of_string (lexeme lexbuf)) }
  | sym { SYM (lexeme lexbuf) }
  | '"' { STR (read_string "" lexbuf) }
  | _ { raise (SyntaxError ("Unexpected character: " ^ lexeme lexbuf)) }
  | eof { EOF }
and read_string s = parse
  | '"' { s }
  | '\\' '\\' { read_string (s ^ "\\") lexbuf }
  | '\\' 't' { read_string (s ^ "\t") lexbuf }
  | '\\' 'n' { read_string (s ^ "\n") lexbuf }
  | '\\' '"' { read_string (s ^ "\"") lexbuf }
  | [^ '"' '\\']+ { read_string (s ^ lexeme lexbuf) lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ lexeme lexbuf)) }
  | eof { raise (SyntaxError "String is not terminated") }

{
  let parse_program lexbuf =
    try Ok (Parser.program read lexbuf)
    with _ -> Error "Parse error"

  let parse_line lexbuf =
    try Ok (Parser.s read lexbuf)
    with _ -> Error "Parse error"
}
