
open Batteries
open Evaluator

type func = t -> (t, err) result

val terminate : func

val push_stack : int -> func

