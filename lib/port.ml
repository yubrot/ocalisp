type port =
  | Read of in_channel
  | Write of out_channel
  | Closed

type t = port ref

let of_in ch = ref (Read ch)

let of_out ch = ref (Write ch)

let close p =
  match !p with
  | Read ch -> close_in ch; p := Closed
  | Write ch -> close_out ch; p := Closed
  | _ -> ()

let to_in p =
  match !p with
  | Read ch -> Some ch
  | _ -> None

let to_out p =
  match !p with
  | Write ch -> Some ch
  | _ -> None
