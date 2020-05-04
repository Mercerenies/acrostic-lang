
open Batteries

type dir = Right | Down

type word =
  { start_pos : int * int;
    end_pos   : int * int;
    text      : string;
  }

let move_in_dir d n (x, y) =
  match d with
  | Right -> (x + n, y)
  | Down  -> (x, y + n)

let letters_in_dir grid d n xy0 =
  let rec navigate xy =
    match CodeGrid.get_cell grid xy with
    | ' ' -> []
    | c   -> c :: navigate (move_in_dir d n xy) in
  String.of_list (navigate xy0)

let word_starting_at grid d xy =
  let forward  = letters_in_dir grid d   1  xy in
  let backward = letters_in_dir grid d (-1) xy in
  if String.length backward = 1 && String.length forward <> 1 then
    Some { start_pos = xy;
           end_pos = move_in_dir d (String.length forward - 1) xy;
           text = forward; }
  else if String.length backward <> 1 && String.length forward = 1 then
    Some { start_pos = move_in_dir d (- (String.length backward - 1)) xy;
           end_pos = xy;
           text = backward; }
  else
    None

let word_starting_at_with_sign grid d xy =
  Option.map (fun word -> if word.start_pos = xy then (word, 1) else (word, -1)) (word_starting_at grid d xy)

let word_at grid d xy =
  let back_count = String.length (letters_in_dir grid d (-1) xy) in
  if back_count <= 0 then
    None
  else
    let xy' = move_in_dir d (- (back_count - 1)) xy in
    word_starting_at grid d xy'

let find_all_words grid =
  let bounds = CodeGrid.bounds_of grid in
  let positions = Enum.cartesian_product (CodeGrid.all_positions bounds) (List.enum [Right; Down]) in
  let lookup_at d pos = word_starting_at_with_sign grid d pos |>
                          Option.filter (fun (_, n) -> n = 1) |>
                          Option.map Tuple2.first in
  positions |> Enum.filter_map (fun (pos, d) -> lookup_at d pos)

let find_word grid f =
  find_all_words grid |> Enum.filter (fun word -> f word.text)

let other_dir x =
  match x with
  | Right -> Down
  | Down -> Right

let position_in_word word (x, y) =
  let (x0, y0) = word.start_pos in
  (x - x0) + (y - y0)

let direction_of word =
  let (x0, _) = word.start_pos in
  let (x1, _) = word.end_pos in
  if x0 <> x1 then Right else Down
