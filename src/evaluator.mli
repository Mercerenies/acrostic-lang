
open Batteries

module FlagMap : Map.S with type key = Flags.t

type t =
  { code: CodeGrid.t;
    pointer: int * int;
    flags: int FlagMap.t;
    dir: WordParser.dir;
    vel: int;
    stack: int list;
    storage: int list; }

type err = StackUnderflow
         | NoSuchWord of string
         | InvalidPosition of (int * int)
         | NoStartPosition
         | AmbiguousStartPosition of (int * int) list
         | MathError of string

val get_flag : Flags.t -> t -> int

val set_flag : Flags.t -> int -> t -> t

val err_to_string : err -> string

module type Dictionary = sig

  val synonyms_of : string -> string list

  val antonyms_of : string -> string list

  val execute_forward : string -> (t -> (t, err) result) option

  val execute_backward : string -> (t -> (t, err) result) option

  val starting_word : string

  val should_acknowledge : WordParser.word -> t -> bool

end

module Eval(Dict : Dictionary) : sig

  val execute_one_step : Debugger.t -> t -> (t, err) result

  val execute_until_done : Debugger.t -> t -> (t, err) result

  val starting_state : CodeGrid.t -> (t, err) result

  val execute_code : Debugger.t -> CodeGrid.t -> (t, err) result

  val check_all_words_exist : CodeGrid.t -> (unit, err) result

end
