type +'a t =
  | Num of float
  | Sym of string
  | Str of string
  | Cons of 'a t * 'a t
  | Nil
  | True
  | False
  | Pure of 'a

let of_list ss =
  List.fold_right (fun a b -> Cons (a, b)) ss Nil

let of_bool = function
  | true -> True
  | false -> False

let rec to_list = function
  | Nil -> Some []
  | Cons (a, b) ->
    begin match to_list b with
      | Some ls -> Some (a :: ls)
      | None -> None
    end
  | _ -> None

let to_bool = function
  | False -> false
  | _ -> true

let quote s =
  of_list [Sym "quote"; s]

let quasiquote s =
  of_list [Sym "quasiquote"; s]

let unquote s =
  of_list [Sym "unquote"; s]

let unquote_splicing s =
  of_list [Sym "unquote-splicing"; s]

let to_string pure_to_string =
  let rec to_string s =
    match s with
    | Num n ->
      if mod_float n 1. = 0. then
        string_of_int (int_of_float n)
      else
        string_of_float n
    | Sym s -> s
    | Str s -> "\"" ^ String.escaped s ^ "\""
    | Cons (Sym "quote", Cons (s, Nil)) -> "'" ^ to_string s
    | Cons (Sym "quasiquote", Cons (s, Nil)) -> "`" ^ to_string s
    | Cons (Sym "unquote", Cons (s, Nil)) -> "," ^ to_string s
    | Cons (Sym "unquote-splicing", Cons (s, Nil)) -> ",@" ^ to_string s
    | Cons (a, b) -> "(" ^ cons_to_string a b ^ ")"
    | Nil -> "()"
    | True -> "#t"
    | False -> "#f"
    | Pure p -> pure_to_string p
  and cons_to_string a b =
    match b with
    | Nil -> to_string a
    | Cons (b, c) -> to_string a ^ " " ^ cons_to_string b c
    | b -> to_string a ^ " . " ^ to_string b
  in to_string
