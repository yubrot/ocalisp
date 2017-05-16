type t 'a = {current: Hashtbl.t string 'a, parent: option (t 'a)};

exception UndefinedVariable string;

let create parent => {current: Hashtbl.create 4, parent};

let def k v env => Hashtbl.replace env.current k v;

let rec set k v env =>
  if (Hashtbl.mem env.current k) {
    Hashtbl.replace env.current k v
  } else {
    switch env.parent {
    | Some e => set k v e
    | None => raise (UndefinedVariable k)
    }
  };

let rec find k env =>
  switch (Hashtbl.find_all env.current k) {
  | [x, ..._] => Some x
  | [] =>
    switch env.parent {
    | Some e => find k e
    | None => None
    }
  };

let rec get k env =>
  switch (find k env) {
  | Some v => v
  | None => raise (UndefinedVariable k)
  };
