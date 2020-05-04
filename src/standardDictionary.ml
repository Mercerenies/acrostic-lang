
open Batteries
open Functions
open Dictionary

let literal_number xs n =
  { forward = xs;
    forward_def = push_stack n;
    backward = [];
    backward_def = push_stack (- n); }

module Words = struct

  let entries = [
      { forward = ["START"; "BEGIN"];
        forward_def = (fun t -> Ok t);
        backward = ["END"; "FINISH"];
        backward_def = terminate; };
      literal_number ["ONE"] 1;
      literal_number ["TWO"] 2;
      literal_number ["THREE"] 3;
      literal_number ["FOUR"] 4;
      literal_number ["FIVE"] 5;
      literal_number ["SIX"] 6;
      literal_number ["SEVEN"] 7;
      literal_number ["EIGHT"] 8;
      literal_number ["NINE"] 9;
      literal_number ["TEN"] 10;
      { forward = ["ASCII"; "TEXTUALLY"; "TEXT"; "CHARACTER"];
        forward_def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_ascii);
        backward = ["NUMBER"; "NUMERICALLY"; "INTEGER"];
        backward_def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_number); };
      { forward = ["PRINT"; "OUTPUT"; "WRITE"];
        forward_def = user_output;
        backward = ["SCAN"; "INPUT"; "READ"];
        backward_def = user_input; };
    ]

  let starting_word = "START"

end

module Dict = Dictionary.Dict(Words)
