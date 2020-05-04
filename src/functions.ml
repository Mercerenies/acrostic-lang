
open Batteries
open Evaluator

type func = t -> (t, err) result

let terminate state = Ok (set_flag Flags.Termination 1 state)
