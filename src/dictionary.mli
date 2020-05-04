
open Batteries

type definition =
  { words: string list;
    def: Evaluator.t -> (Evaluator.t, Evaluator.err) result; }

type entry =
  { forward: definition;
    backward: definition; }

val reversed : entry -> entry

val self_opposite : definition -> entry

module type WordList = sig
  val entries : entry list
  val starting_word : string
end

module Dict(W : WordList) : Evaluator.Dictionary
