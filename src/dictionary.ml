
open Batteries

type definition =
  { words: string list;
    def: Evaluator.t -> (Evaluator.t, Evaluator.err) result; }

type entry =
  { forward: definition;
    backward: definition; }

let reversed e =
  { forward = e.backward;
    backward = e.forward; }

let self_opposite d =
  { forward = d;
    backward = d; }

module type WordList = sig
  val entries : entry list
  val starting_word : string
end

module Dict(W : WordList) = struct

  module WordMap = Map.Make(String)

  let word_map =
    let go m x =
      let xrev = reversed x in
      let m' = List.fold_left (fun m1 w -> WordMap.add w x m1) m x.forward.words in
      let m'' = List.fold_left (fun m1 w -> WordMap.add w xrev m1) m' x.backward.words in
      m'' in
    List.fold_left go WordMap.empty W.entries

  let synonyms_of s =
    WordMap.find_opt s word_map |>
      Option.map_default (fun x -> x.forward.words) []

  let antonyms_of s =
    WordMap.find_opt s word_map |>
      Option.map_default (fun x -> x.backward.words) []

  let execute_forward s =
    WordMap.find_opt s word_map |>
      Option.map (fun x -> x.forward.def)

  let execute_backward s =
    WordMap.find_opt s word_map |>
      Option.map (fun x -> x.backward.def)

  let starting_word = W.starting_word

  let should_acknowledge _ _ = true

end
