
open Batteries
open Evaluator

type func = t -> (t, err) result

let (>>=) = Result.Monad.(>>=)

let terminate state = Ok (set_flag Flags.Termination 1 state)

let push_stack n state =
  Ok { state with stack = n :: state.stack; }

let pop_storage state =
  match state.storage with
  | [] -> Error StackUnderflow
  | (x :: xs) -> Ok (x, { state with storage = xs; })

let push_storage n state =
  Ok { state with storage = n :: state.storage; }

let pop_stack state =
  match state.stack with
  | [] -> Error StackUnderflow
  | (x :: xs) -> Ok (x, { state with stack = xs; })

let successfully f state = Ok (f state)

let noop = successfully identity

let dup state =
  pop_stack state >>= fun (x, state') ->
  push_stack x state' >>= fun state'' ->
  push_stack x state''

let user_input state =
  let mode = get_flag Flags.IOMode state in
  if mode = Flags.io_mode_ascii then
    let char = input_char stdin in
    push_stack (Char.code char) state
  else
    let value = read_int () in
    push_stack value state

let user_output state =
  pop_stack state >>= fun (i, state') ->
  let mode = get_flag Flags.IOMode state in
  if mode = Flags.io_mode_ascii then
    print_char (Char.chr i)
  else
    print_int i;
  flush stdout;
  Ok state'

let unary_op f state =
  pop_stack state >>= fun (x, state') ->
  push_stack (f x) state'

let binary_op f state =
  pop_stack state >>= fun (x, state') ->
  pop_stack state' >>= fun (y, state'') ->
  push_stack (f x y) state''

let safe_div state =
  pop_stack state >>= fun (x, state') ->
  pop_stack state' >>= fun (y, state'') ->
  try
    push_stack (x / y) state'' >>= push_stack (x mod y) (* TODO Make this truncate toward negative infinity *)
  with Division_by_zero -> Error (MathError "division by zero")

let swap state =
  pop_stack state >>= fun (x, state') ->
  pop_stack state' >>= fun (y, state'') ->
  push_stack x state'' >>= push_stack y

let store_value state =
  pop_stack state >>= fun (x, state') ->
  push_storage x state'

let retrieve_value state =
  pop_storage state >>= fun (x, state') ->
  push_stack x state'
