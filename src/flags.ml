
type t = Termination
       | IOMode

let int t =
  match t with
  | Termination -> 0
  | IOMode -> 1

let of_int n =
  match n with
  | 0 -> Some Termination
  | 1 -> Some IOMode
  | _ -> None

let compare a b = Int.compare (int a) (int b)

let io_mode_number = 0

let io_mode_ascii = 1
