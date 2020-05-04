
type t = Termination

let int t =
  match t with
  | Termination -> 0

let of_int n =
  match n with
  | 0 -> Some Termination
  | _ -> None

let compare a b = Int.compare (int a) (int b)

