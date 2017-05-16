type t +'a =
  | Num float
  | Sym string
  | Str string
  | Cons (t 'a) (t 'a)
  | Nil
  | Bool bool
  | Pure 'a;

let of_list: list (t 'a) => t 'a;

let to_list: t 'a => option (list (t 'a));

let test: t 'a => bool;

let quote: t 'a => t 'a;

let quasiquote: t 'a => t 'a;

let unquote: t 'a => t 'a;

let unquote_splicing: t 'a => t 'a;

let to_string: ('a => string) => t 'a => string;
