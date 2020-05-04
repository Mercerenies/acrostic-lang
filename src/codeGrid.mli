
type t

val make_grid : int * int -> t

val width_of : t -> int

val height_of : t -> int

val bounds_of : t -> int * int

val get_cell : t -> int * int -> char

val set_cell : t -> int * int -> char -> t

val mapi : (int * int -> char -> char) -> t -> t

val to_string : t -> string

val of_list : string list -> t

val all_positions : (int * int) -> (int * int) BatEnum.t
