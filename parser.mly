%token <float> NUM
%token <string> SYM
%token <string> STR
%token LPAREN RPAREN LBRACK RBRACK DOT TRUE FALSE QUOTE QUASIQUOTE UNQUOTE UNQUOTE_SPLICING EOF

%start program
%type <'a Sexp.t list> program
%start s
%type <'a Sexp.t> s

%%

program:
  | rev_ss EOF { List.rev $1 }
s:
  | LPAREN RPAREN { Sexp.Nil }
  | LBRACK RBRACK { Sexp.Nil }
  | LPAREN s_inner RPAREN { $2 }
  | LBRACK s_inner RBRACK { $2 }
  | s_quoted { $1 }
  | NUM { Sexp.Num $1 }
  | SYM { Sexp.Sym $1 }
  | STR { Sexp.Str $1 }
  | TRUE { Sexp.True }
  | FALSE { Sexp.False }
s_quoted:
  | QUOTE s { Sexp.quote $2 }
  | QUASIQUOTE s { Sexp.quasiquote $2 }
  | UNQUOTE s { Sexp.unquote $2 }
  | UNQUOTE_SPLICING s { Sexp.unquote_splicing $2 }
s_inner:
  | rev_ss_1 s_inner_term { List.fold_left (fun a b -> Sexp.Cons (b, a)) $2 $1 }
s_inner_term:
  | { Sexp.Nil }
  | DOT s { $2 }
rev_ss_1:
  | rev_ss s { $2 :: $1 }
rev_ss:
  | { [] }
  | rev_ss s { $2 :: $1 }
