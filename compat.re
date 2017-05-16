// Currently (2017/05) Reason runs on OCaml 4.02.3,
// which lacks some definitions used in original ocalisp.

type result 'a 'b =
  | Ok 'a
  | Error 'b;

let module String = {
  include String;

  let rec split_on_char (sep: char) (s: string) : list string =>
    try {
      let i = index s sep;
      let j = i + 1;
      let (a, b) = (sub s 0 i, sub s j (length s - j));
      [a, ...split_on_char sep b]
    } {
    | Not_found => [s]
    };
};
