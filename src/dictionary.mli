
open Batteries

type entry = {
    forward: string list;
    forward_def: Evaluator.t -> (Evaluator.t, Evaluator.err) result;
    backward: string list;
    backward_def: Evaluator.t -> (Evaluator.t, Evaluator.err) result;
  }

val reversed : entry -> entry

module type WordList = sig
  val entries : entry list
  val starting_word : string
end

module Dict(W : WordList) : Evaluator.Dictionary
