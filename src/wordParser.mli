
open Batteries

type dir = Right | Down

type word =
  { start_pos : int * int;
    end_pos   : int * int;
    text      : string;
  }

val move_in_dir : dir -> int -> int * int -> int * int

val word_starting_at : CodeGrid.t -> dir -> int * int -> word option

val word_starting_at_with_sign : CodeGrid.t -> dir -> int * int -> (word * int) option

val word_at : CodeGrid.t -> dir -> int * int -> word option

val find_all_words : CodeGrid.t -> word Enum.t

val find_word : CodeGrid.t -> (string -> bool) -> word Enum.t

val other_dir : dir -> dir

val position_in_word : word -> int * int -> int

val direction_of : word -> dir
