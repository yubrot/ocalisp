type t +'a =
  | Num float
  | Sym string
  | Str string
  | Cons (t 'a) (t 'a)
  | Nil
  | Bool bool
  | Pure 'a;

let of_list ss => List.fold_right (fun a b => Cons a b) ss Nil;

let rec to_list =
  fun
  | Nil => Some []
  | Cons a b =>
    switch (to_list b) {
    | Some ls => Some [a, ...ls]
    | None => None
    }
  | _ => None;

let test =
  fun
  | Bool b => b
  | _ => true;

let quote s => of_list [Sym "quote", s];

let quasiquote s => of_list [Sym "quasiquote", s];

let unquote s => of_list [Sym "unquote", s];

let unquote_splicing s => of_list [Sym "unquote-splicing", s];

let to_string pure_to_string => {
  let rec to_string s =>
    switch s {
    | Num n =>
      if (mod_float n 1. == 0.) {
        string_of_int (int_of_float n)
      } else {
        string_of_float n
      }
    | Sym s => s
    | Str s => "\"" ^ String.escaped s ^ "\""
    | Cons (Sym "quote") (Cons s Nil) => "'" ^ to_string s
    | Cons (Sym "quasiquote") (Cons s Nil) => "`" ^ to_string s
    | Cons (Sym "unquote") (Cons s Nil) => "," ^ to_string s
    | Cons (Sym "unquote-splicing") (Cons s Nil) => ",@" ^ to_string s
    | Cons a b => "(" ^ cons_to_string a b ^ ")"
    | Nil => "()"
    | Bool true => "#t"
    | Bool false => "#f"
    | Pure p => pure_to_string p
    }
  and cons_to_string a b =>
    switch b {
    | Nil => to_string a
    | Cons b c => to_string a ^ " " ^ cons_to_string b c
    | b => to_string a ^ " . " ^ to_string b
    };
  to_string
};
