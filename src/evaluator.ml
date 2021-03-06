
open Batteries

module FlagMap = Map.Make(Flags)

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

let dump_state state =
  let (x, y) = state.pointer in
  let dir = if state.dir = Right then "Right" else "Down" in
  Printf.printf "(%d, %d) %s %d\n" x y dir state.vel

let get_flag f t = Option.default 0 @@ FlagMap.find_opt f t.flags

let set_flag f i t = { t with flags = FlagMap.add f i t.flags; }

let err_to_string e =
  let open Printf in
  match e with
  | StackUnderflow -> "Stack underflow"
  | NoSuchWord s -> sprintf "No such word '%s'" s
  | InvalidPosition (x, y) -> sprintf "Invalid position (%d, %d)" x y
  | NoStartPosition -> "No starting position"
  | MathError s -> "Math error: " ^ s
  | AmbiguousStartPosition xs ->
     let xs' = List.map (fun (x, y) -> sprintf "(%d, %d)" x y) xs in
     "Ambiguous starting position: " ^ String.concat ", " xs'

module type Dictionary = sig

  val synonyms_of : string -> string list

  val antonyms_of : string -> string list

  val execute_forward : string -> (t -> (t, err) result) option

  val execute_backward : string -> (t -> (t, err) result) option

  val starting_word : string

  val should_acknowledge : WordParser.word -> t -> bool

end

let at_invalid_pos state =
  CodeGrid.get_cell state.code state.pointer = ' '

let print_state_of_system debug state word =
  if Debugger.int debug >= Debugger.int Debugger.PrintEverything then
    begin
      flush stdout;
      Debugger.print_stack "Value" state.stack debug;
      Debugger.print_stack "Storage" state.storage debug;
      print_endline word.WordParser.text
    end

module Eval(Dict : Dictionary) = struct

  module ResultOps = Util.MonadOps(struct
                         type 'a t = ('a, err) result
                         include Result.Monad
                       end)

  let (>>=) = Result.Monad.(>>=)

  let rec execute_one_step debug state =
    let open WordParser in
    if at_invalid_pos state then
      Error (InvalidPosition state.pointer)
    else
      let dir = WordParser.other_dir state.dir in
      let at_pos = WordParser.word_at state.code dir state.pointer in
      match at_pos with
      | Some word when Dict.should_acknowledge word state -> begin
          let rel_pos = WordParser.position_in_word word state.pointer in
          print_state_of_system debug state word;
          let (state', f) =
            if 2 * rel_pos < String.length word.text then
              (* Forward execution *)
              let state' = { state with pointer = word.end_pos;
                                        dir = dir;
                                        vel = 1; } in
              (state', Dict.execute_forward word.text)
            else
              (* Backward execution *)
              let state' = { state with pointer = word.start_pos;
                                        dir = dir;
                                        vel = -1; } in
              (state', Dict.execute_backward word.text) in
          match f with
          | None -> Error (NoSuchWord word.text)
          | Some f' -> f' state'
        end
      | _ ->
         execute_one_step
           debug
           { state with pointer = WordParser.move_in_dir state.dir (- state.vel) state.pointer; }

  let rec execute_until_done debug state =
    if get_flag Flags.Termination state <> 0 then
      Ok state
    else
    execute_one_step debug state >>= execute_until_done debug

  let starting_state code =
    let open WordParser in
    let starting_words = Dict.synonyms_of Dict.starting_word in
    let ending_words = Dict.antonyms_of Dict.starting_word in
    let matches = WordParser.find_word code (fun x -> List.mem x starting_words || List.mem x ending_words) in
    match List.of_enum matches with
    | [word] when List.mem word.text starting_words ->
       Ok { code = code;
            pointer = word.end_pos;
            flags = FlagMap.empty;
            dir = WordParser.direction_of word;
            vel = 1;
            stack = [];
            storage = []; }
    | [word] when List.mem word.text ending_words ->
       Ok { code = code;
            pointer = word.start_pos;
            flags = FlagMap.empty;
            dir = WordParser.direction_of word;
            vel = -1;
            stack = [];
            storage = []; }
    | [] -> Error NoStartPosition
    | xs ->
       let start_or_end x = if List.mem x.text starting_words then x.start_pos else x.end_pos in
       Error (AmbiguousStartPosition (List.map start_or_end xs))

  let execute_code debug code =
      starting_state code >>= execute_until_done debug

  let check_all_words_exist code =
    let open WordParser in
    let words = WordParser.find_all_words code in
    let f () b =
      match Dict.execute_forward b.text with
      | Some _ -> Ok ()
      | None -> Error (NoSuchWord b.text) in
    ResultOps.fold f () words

end
