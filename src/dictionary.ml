
open Batteries

type entry = {
    forward: string list;
    forward_def: Evaluator.t -> (Evaluator.t, Evaluator.err) result;
    backward: string list;
    backward_def: Evaluator.t -> (Evaluator.t, Evaluator.err) result;
  }

let reversed e =
  { forward = e.backward;
    forward_def = e.backward_def;
    backward = e.forward;
    backward_def = e.forward_def; }

let self_opposite xs f =
  { forward = xs;
    forward_def = f;
    backward = xs;
    backward_def = f; }

module type WordList = sig
  val entries : entry list
  val starting_word : string
end

module Dict(W : WordList) = struct

  module WordMap = Map.Make(String)

  let word_map =
    let go m x =
      let xrev = reversed x in
      let m' = List.fold_left (fun m1 w -> WordMap.add w x m1) m x.forward in
      let m'' = List.fold_left (fun m1 w -> WordMap.add w xrev m1) m' x.backward in
      m'' in
    List.fold_left go WordMap.empty W.entries

  let synonyms_of s =
    WordMap.find_opt s word_map |>
      Option.map_default (fun x -> x.forward) []

  let antonyms_of s =
    WordMap.find_opt s word_map |>
      Option.map_default (fun x -> x.backward) []

  let execute_forward s =
    WordMap.find_opt s word_map |>
      Option.map (fun x -> x.forward_def)

  let execute_backward s =
    WordMap.find_opt s word_map |>
      Option.map (fun x -> x.backward_def)

  let starting_word = W.starting_word

end
