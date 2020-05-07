
open Batteries

type definition =
  { words: string list;
    def: Evaluator.t -> (Evaluator.t, Evaluator.err) result;
    doc: string; }

type branch_policy = Evaluator.t -> bool

type entry =
  { forward: definition;
    backward: definition;
    forward_branch: branch_policy;
    backward_branch: branch_policy; }

val no_branch : branch_policy

val reversed : entry -> entry

val self_opposite : definition -> entry

module type WordList = sig
  val entries : entry list
  val starting_word : string
end

module Dict(W : WordList) : Evaluator.Dictionary
