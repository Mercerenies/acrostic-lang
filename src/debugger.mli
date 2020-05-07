
type t = NoDebug
       | PrintStack
       | PrintEverything

val int : t -> int

val of_int : int -> t option

val compare : t -> t -> int

val print_stack : string -> int list -> t -> unit
