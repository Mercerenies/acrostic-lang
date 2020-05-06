
open Batteries

type definition =
  { words: string list;
    def: Evaluator.t -> (Evaluator.t, Evaluator.err) result; }

type branch_policy = Evaluator.t -> bool

type entry =
  { forward: definition;
    backward: definition;
    forward_branch: branch_policy;
    backward_branch: branch_policy; }

let no_branch _ = true

let reversed e =
  { forward = e.backward;
    backward = e.forward;
    forward_branch = e.backward_branch;
    backward_branch = e.forward_branch; }

let self_opposite d =
  { forward = d;
    backward = d;
    forward_branch = no_branch;
    backward_branch = no_branch; }

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

  let should_acknowledge word state =
    let open WordParser in
    let rel_pos = WordParser.position_in_word word state.Evaluator.pointer in
    let dir = if 2 * rel_pos < String.length word.text then 1 else -1 in
    WordMap.find_opt word.text word_map |>
      Option.map_default
        (fun x -> if dir > 0 then x.forward_branch state else x.backward_branch state)
        false

end
