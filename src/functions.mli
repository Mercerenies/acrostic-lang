
open Batteries
open Evaluator

type func = t -> (t, err) result

val terminate : func

val push_stack : int -> func

val successfully : (t -> t) -> func

val pop_stack : t -> (int * t, err) result

val user_input : func

val user_output : func
