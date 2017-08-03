type 'a t = {
  current: (string, 'a) Hashtbl.t;
  parent: 'a t option;
}

exception UndefinedVariable of string

let create parent =
  { current = Hashtbl.create 4; parent; }

let def k v env =
  Hashtbl.replace env.current k v

let rec set k v env =
  if Hashtbl.mem env.current k then
    Hashtbl.replace env.current k v
  else match env.parent with
    | Some e -> set k v e
    | None -> raise (UndefinedVariable k)

let rec find k env =
  match Hashtbl.find_all env.current k with
  | x :: _ -> Some x
  | [] -> match env.parent with
    | Some e -> find k e
    | None -> None

let rec get k env =
  match find k env with
  | Some v -> v
  | None -> raise (UndefinedVariable k)
