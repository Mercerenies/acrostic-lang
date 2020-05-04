
open Batteries
open Evaluator

type func = t -> (t, err) result

val terminate : func

val push_stack : int -> func

val pop_stack : t -> (int * t, err) result

val push_storage : int -> func

val pop_storage : t -> (int * t, err) result

val successfully : (t -> t) -> func

val noop : func

val dup : func

val user_input : func

val user_output : func

val unary_op : (int -> int) -> func

(* Top of stack is second arg *)
val binary_op : (int -> int -> int) -> func

val safe_div : func

val swap : func

val store_value : func

val retrieve_value : func
