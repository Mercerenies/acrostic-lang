
open Batteries
open Functions
open Dictionary

let literal_number xs n =
  { forward =
      { words = xs;
        def = push_stack n; };
    backward =
        { words = [];
          def = push_stack (- n); }; }

let entry f b = { forward = f; backward = b; }

module Words = struct

  let entries = [
      entry
        { words = ["START"; "BEGIN"; "STARTING"; "BEGINNING"; "STARTED"; "STARTS";
                   "BEGINS"; "BEGAN"; "BEGUN"];
          def = (fun t -> Ok t); }
        { words = ["END"; "FINISH"; "ENDING"; "FINISHING"; "ENDS"; "FINISHES"; "ENDED";
                   "FINISHED"];
          def = terminate; };
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
      entry
        { words = ["ASCII"; "TEXTUALLY"; "TEXT"; "CHARACTER"; "TEXTUAL"; "TEXTS"; "STRING";
                   "CHARACTERS"; "STRINGS"];
          def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_ascii); }
        { words = ["NUMBER"; "NUMERICALLY"; "INTEGER"; "NUMBERS"; "INTEGERS"; "NUMERICAL"];
          def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_number); };
      entry
        { words = ["PRINT"; "OUTPUT"; "WRITE"; "PRINTS"; "PRINTING"; "PRINTED"; "OUTPUTS";
                   "OUTPUTTING"; "OUTPUTTED"; "WRITES"; "WROTE"; "WRITING"];
          def = user_output; }
        { words = ["SCAN"; "INPUT"; "READ"; "SCANNED"; "SCANNING"; "SCANS"; "INPUTTED"; "INPUTTING";
                   "INPUTS"; "READS"; "READING"];
          def = user_input; };
      entry
        { words = ["ADD"; "ADDITION"; "SUM"; "COMBINE"; "COMBINED"; "ADDING"; "ADDED"; "ADDITIVE";
                   "COMBINING"; "SUMMING"; "SUMMED"; "ADDS"; "COMBINES"; "SUMS"];
          def = binary_op (+); }
        { words = ["SUBTRACT"; "SUBTRACTION"; "DIFFERENCE"; "WITHOUT"; "SUBTRACTING"; "SUBTRACTED";
                   "SUBTRACTS"; "DIFFER"; "DIFFERING"; "DIFFERS"; "DIFFERED"];
          def = binary_op (-); };
      entry
        { words = ["MULTIPLY"; "MULTIPLYING"; "MULTIPLIED"; "MULTIPLIES"; "TIMES"; "OF"];
          def = binary_op ( * ); }
        { words = ["DIVIDE"; "DIVIDING"; "DIVIDED"; "DIVIDES"; "QUOTIENT"; "MODULO"; "BY";
                   "REMAINDER"; "REMAINDERS"];
          def = safe_div; };
      self_opposite
        { words = ["NOTHING"; "VOID"; "NULL"; "EMPTY"; "WAIT"; "STANDBY"; "REST";
                   "NOTHINGNESS"; "WAITING"; "RESTING"; "EMPTINESS"; "VOIDS"; "NULLS";
                   "EMPTINESS"; "WAITS"; "WAITED"; "RESTED"; "RESTS"; "NIL"; "NILS"];
          def = noop; };
      entry
        { words = ["DUPLICATE"; "DITTO"; "AGAIN"; "CLONE"; "COPY"; "DUPLICATING"; "DUPLICATED";
                   "DUPLICATES"; "CLONING"; "CLONED"; "CLONES"; "COPYING"; "COPIED"; "COPIES"];
          def = dup; }
        { words = ["POP"; "REMOVE"; "POPPING"; "POPS"; "POPPED"; "REMOVING"; "REMOVES"; "REMOVED";
                   "DELETE"; "DELETING"; "DELETES"; "DELETED"; "DISCARD"; "DISCARDING"; "DISCARDED";
                   "DISCARDS"];
          def = fun state -> Result.map (fun (_, s) -> s) @@ pop_stack state; };
      self_opposite
        { words = ["SWAP"; "SWAPPING"; "SWAPPED"; "SWAPS"; "FLIP"; "FLIPPING";
                   "FLIPS"; "FLIPPED"; "SWITCH"; "SWITCHED"; "SWITCHING"; "SWITCHES";
                   "EXCHANGE"; "EXCHANGES"; "EXCHANGING"; "EXCHANGED"];
          def = swap; };
      entry
        { words = ["STORE"; "PUT"; "STORES"; "STORING"; "STORED"; "PUTS"; "PUTTING";
                   "HOARD"; "HOARDS"; "HOARDING"; "HOARDED"; "KEEP"; "KEEPS"; "KEEPING";
                   "KEPT"; "STASH"; "STASHED"; "STASHING"; "STASHES"; "DEPOSIT"; "DEPOSITING";
                   "DEPOSITED"; "DEPOSITS"];
          def = store_value; }
        { words = ["RETRIEVE"; "RETRIEVES"; "RETRIEVED"; "RETRIEVING"; "GET"; "GETS"; "GETTING";
                   "GOT"; "WITHDRAW"; "WITHDRAWING"; "WITHDRAWS"; "WITHDREW"; "WITHDRAWAL";
                   "WITHDRAWALS"; "RECALL"; "RECALLS"; "RECALLING"; "RECALLED"; "RECLAIM";
                   "RECLAIMS"; "RECLAIMING"; "RECLAIMED"; "FETCH"; "FETCHED"; "FETCHING";
                   "FETCHES"];
          def = retrieve_value; };
    ]

  let starting_word = "START"

end

module Dict = Dictionary.Dict(Words)
