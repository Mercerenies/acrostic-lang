
open Batteries

type t = Grid of (int * int * char Vect.t)

let index_to_pos (Grid (w, _, _)) i = (i mod w, i / w)

let pos_to_index (Grid (w, _, _)) (x, y) = y * w + x

let make_grid (w, h) = Grid (w, h, Vect.make (w * h) ' ')

let width_of (Grid (w, _, _)) = w

let height_of (Grid (_, h, _)) = h

let bounds_of (Grid (w, h, _)) = (w, h)

let get_cell (Grid (w, h, arr)) (x, y) =
  if x < 0 || y < 0 || x >= w || y >= h then
    ' '
  else
    try
      Vect.get arr (pos_to_index (Grid (w, h, arr)) (x, y))
    with Vect.Out_of_bounds -> ' '

let set_cell (Grid (w, h, arr)) xy c =
  let arr' = Vect.set arr (pos_to_index (Grid (w, h, arr)) xy) c in
  Grid (w, h, arr')

let mapi f (Grid (w, h, arr)) =
  let f' i x = f (index_to_pos (Grid (w, h, arr)) i) x in
  Grid (w, h, Vect.mapi f' arr)

let map f grid = mapi (fun _ -> f) grid

let to_string (Grid (w, _, arr)) =
  let f i c =
    if i mod w = w - 1 then String.make 1 c ^ "\n" else String.make 1 c in
  Vect.mapi f arr |> Vect.to_list |> String.concat ""

let of_list lst =
  let width = lst |> List.map String.length |> List.max in
  let height = List.length lst in
  let padded = List.map (Util.pad_right width ' ') lst in
  Grid (width, height, Vect.of_enum (String.enum (String.concat "" padded)))

let all_positions (x, y) =
  Enum.cartesian_product (Enum.range 0 ~until:(x-1)) (Enum.range 0 ~until:(y-1))
