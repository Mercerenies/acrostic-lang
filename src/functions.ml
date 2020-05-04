
open Batteries
open Evaluator

type func = t -> (t, err) result

let (>>=) = Result.Monad.(>>=)

let terminate state = Ok (set_flag Flags.Termination 1 state)

let push_stack n state =
  Ok { state with stack = n :: state.stack; }

let successfully f state = Ok (f state)

let pop_stack state =
  match state.stack with
  | [] -> Error StackUnderflow
  | (x :: xs) -> Ok (x, { state with stack = xs; })

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
  Ok state'

let unary_op f state =
  pop_stack state >>= fun (x, state') ->
  push_stack (f x) state'

let binary_op f state =
  pop_stack state >>= fun (x, state') ->
  pop_stack state' >>= fun (y, state'') ->
  push_stack (f x y) state''
