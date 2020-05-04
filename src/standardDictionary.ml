
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
      { forward = ["START"; "BEGIN"; "STARTING"; "BEGINNING"; "STARTED"; "STARTS";
                   "BEGINS"; "BEGAN"; "BEGUN"];
        forward_def = (fun t -> Ok t);
        backward = ["END"; "FINISH"; "ENDING"; "FINISHING"; "ENDS"; "FINISHES"; "ENDED";
                    "FINISHED"];
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
      { forward = ["ASCII"; "TEXTUALLY"; "TEXT"; "CHARACTER"; "TEXTUAL"; "TEXTS"; "STRING";
                   "CHARACTERS"; "STRINGS"];
        forward_def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_ascii);
        backward = ["NUMBER"; "NUMERICALLY"; "INTEGER"; "NUMBERS"; "INTEGERS"; "NUMERICAL"];
        backward_def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_number); };
      { forward = ["PRINT"; "OUTPUT"; "WRITE"; "PRINTS"; "PRINTING"; "PRINTED"; "OUTPUTS";
                   "OUTPUTTING"; "OUTPUTTED"; "WRITES"; "WROTE"; "WRITING"];
        forward_def = user_output;
        backward = ["SCAN"; "INPUT"; "READ"; "SCANNED"; "SCANNING"; "SCANS"; "INPUTTED"; "INPUTTING";
                    "INPUTS"; "READS"; "READING"];
        backward_def = user_input; };
      { forward = ["ADD"; "ADDITION"; "SUM"; "COMBINE"; "COMBINED"; "ADDING"; "ADDED"; "ADDITIVE";
                   "COMBINING"; "SUMMING"; "SUMMED"; "ADDS"; "COMBINES"; "SUMS"];
        forward_def = binary_op (+);
        backward = ["SUBTRACT"; "SUBTRACTION"; "DIFFERENCE"; "WITHOUT"; "SUBTRACTING"; "SUBTRACTED";
                    "SUBTRACTS"; "DIFFER"; "DIFFERING"; "DIFFERS"; "DIFFERED"];
        backward_def = binary_op (-); };
      { forward = ["MULTIPLY"; "MULTIPLYING"; "MULTIPLIED"; "MULTIPLIES"; "TIMES"; "OF"];
        forward_def = binary_op ( * );
        backward = ["DIVIDE"; "DIVIDING"; "DIVIDED"; "DIVIDES"; "QUOTIENT"; "MODULO"; "BY";
                    "REMAINDER"; "REMAINDERS"];
        backward_def = safe_div; };
      self_opposite ["NOTHING"; "VOID"; "NULL"; "EMPTY"; "WAIT"; "STANDBY"; "REST";
                     "NOTHINGNESS"; "WAITING"; "RESTING"; "EMPTINESS"; "VOIDS"; "NULLS";
                     "EMPTINESS"; "WAITS"; "WAITED"; "RESTED"; "RESTS"; "NIL"; "NILS"] noop;
      { forward = ["DUPLICATE"; "DITTO"; "AGAIN"; "CLONE"; "COPY"; "DUPLICATING"; "DUPLICATED";
                   "DUPLICATES"; "CLONING"; "CLONED"; "CLONES"; "COPYING"; "COPIED"; "COPIES"];
        forward_def = dup;
        backward = ["POP"; "REMOVE"; "POPPING"; "POPS"; "POPPED"; "REMOVING"; "REMOVES"; "REMOVED";
                    "DELETE"; "DELETING"; "DELETES"; "DELETED"; "DISCARD"; "DISCARDING"; "DISCARDED";
                    "DISCARDS"];
        backward_def = fun state -> Result.map (fun (_, s) -> s) @@ pop_stack state; };
      self_opposite ["SWAP"; "SWAPPING"; "SWAPPED"; "SWAPS"; "FLIP"; "FLIPPING";
                     "FLIPS"; "FLIPPED"; "SWITCH"; "SWITCHED"; "SWITCHING"; "SWITCHES";
                     "EXCHANGE"; "EXCHANGES"; "EXCHANGING"; "EXCHANGED"] swap;
      { forward = ["STORE"; "PUT"; "STORES"; "STORING"; "STORED"; "PUTS"; "PUTTING";
                   "HOARD"; "HOARDS"; "HOARDING"; "HOARDED"; "KEEP"; "KEEPS"; "KEEPING";
                   "KEPT"; "STASH"; "STASHED"; "STASHING"; "STASHES"; "DEPOSIT"; "DEPOSITING";
                   "DEPOSITED"; "DEPOSITS"];
        forward_def = store_value;
        backward = ["RETRIEVE"; "RETRIEVES"; "RETRIEVED"; "RETRIEVING"; "GET"; "GETS"; "GETTING";
                    "GOT"; "WITHDRAW"; "WITHDRAWING"; "WITHDRAWS"; "WITHDREW"; "WITHDRAWAL";
                    "WITHDRAWALS"; "RECALL"; "RECALLS"; "RECALLING"; "RECALLED"; "RECLAIM";
                    "RECLAIMS"; "RECLAIMING"; "RECLAIMED"; "FETCH"; "FETCHED"; "FETCHING";
                    "FETCHES"];
        backward_def = retrieve_value; };
    ]

  let starting_word = "START"

end

module Dict = Dictionary.Dict(Words)
