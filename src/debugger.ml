
type t = NoDebug
       | PrintStack
       | PrintEverything

let int x =
  match x with
  | NoDebug -> 0
  | PrintStack -> 1
  | PrintEverything -> 2

let of_int x =
  match x with
  | 0 -> Some NoDebug
  | 1 -> Some PrintStack
  | 2 -> Some PrintEverything
  | _ -> None

let compare a b = Int.compare (int a) (int b)

let print_stack s stack dbg =
  let open Printf in
  if int dbg >= int PrintStack then
    begin
      print_endline (s ^ " stack:\n");
      List.iter (fun i -> printf "  %i\n" i) stack
    end

