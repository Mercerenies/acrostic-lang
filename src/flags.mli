
type t = Termination
       | IOMode

val int : t -> int

val of_int : int -> t option

val compare : t -> t -> int

val io_mode_number : int

val io_mode_ascii : int
