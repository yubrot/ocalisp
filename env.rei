type t 'a;

exception UndefinedVariable string;

let create: option (t 'a) => t 'a;

let def: string => 'a => t 'a => unit;

let set: string => 'a => t 'a => unit;

let find: string => t 'a => option 'a;

let get: string => t 'a => 'a;
