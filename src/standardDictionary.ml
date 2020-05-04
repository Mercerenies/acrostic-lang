
open Batteries
open Functions
open Dictionary

let literal_number xs n =
  { forward =
      { words = xs;
        def = push_stack n; };
    backward =
        { words = [];
          def = push_stack (- n); };
    forward_branch = no_branch;
    backward_branch = no_branch; }

let entry f b = { forward = f; backward = b; forward_branch = no_branch; backward_branch = no_branch }

let branch_top f b entry =
  let with_top g state =
    match pop_stack state with
    | Ok (x, _) -> g x
    | Error _ -> false in
  { entry with
      forward_branch = with_top f;
      backward_branch = with_top b; }

module Words = struct

  let entries = [
      entry
        { words = ["START"; "BEGIN"; "STARTING"; "BEGINNING"; "STARTED"; "STARTS";
                   "BEGINS"; "BEGAN"; "BEGUN"];
          def = (fun t -> Ok t); }
        { words = ["END"; "FINISH"; "ENDING"; "FINISHING"; "ENDS"; "FINISHES"; "ENDED";
                   "FINISHED"];
          def = terminate; };
      literal_number ["ZERO"; "ZILCH"; "NONE"] 0;
      literal_number ["ONE"; "SINGLE"; "SINGULAR"] 1;
      literal_number ["TWO"; "PAIR"; "DOUBLE"; "DUO"] 2;
      literal_number ["THREE"; "TRIPLE"; "TRIO"] 3;
      literal_number ["FOUR"; "QUADRUPLE"] 4;
      literal_number ["FIVE"; "QUINTUPLE"] 5;
      literal_number ["SIX"] 6;
      literal_number ["SEVEN"] 7;
      literal_number ["EIGHT"] 8;
      literal_number ["NINE"] 9;
      literal_number ["TEN"; "DECADE"; "DECADES"] 10;
      literal_number ["ELEVEN"] 11;
      literal_number ["TWELVE"; "DOZEN"] 12;
      literal_number ["THIRTEEN"] 13;
      literal_number ["FOURTEEN"] 14;
      literal_number ["FIFTEEN"] 15;
      literal_number ["SIXTEEN"] 16;
      literal_number ["SEVENTEEN"] 17;
      literal_number ["EIGHTEEN"] 18;
      literal_number ["NINETEEN"] 19;
      literal_number ["TWENTY"; "FORTNIGHT"] 20;
      literal_number ["QUARTER"] 25;
      literal_number ["THIRTY"] 30;
      literal_number ["FORTY"] 40;
      literal_number ["FIFTY"] 50;
      literal_number ["SIXTY"] 60;
      literal_number ["SEVENTY"] 70;
      literal_number ["EIGHTY"] 80;
      literal_number ["NINETY"] 90;
      literal_number ["HUNDRED"; "CENTURY"] 100;
      entry
        { words = ["ASCII"; "TEXTUALLY"; "TEXT"; "CHARACTER"; "TEXTUAL"; "TEXTS"; "STRING";
                   "CHARACTERS"; "STRINGS"];
          def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_ascii); }
        { words = ["NUMBER"; "NUMERICALLY"; "INTEGER"; "NUMBERS"; "INTEGERS"; "NUMERICAL"];
          def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_number); };
      entry
        { words = ["PRINT"; "OUTPUT"; "WRITE"; "PRINTS"; "PRINTING"; "PRINTED"; "OUTPUTS";
                   "OUTPUTTING"; "OUTPUTTED"; "WRITES"; "WROTE"; "WRITING"; "DISPLAY"; "DISPLAYS";
                   "DISPLAYING"; "DISPLAYED"];
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
                   "EMPTINESS"; "WAITS"; "WAITED"; "RESTED"; "RESTS"; "NIL"; "NILS";
                   "VOIDED"; "VOIDING"];
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
      branch_top ((<>) 0) ((=) 0) @@
        entry
          { words = ["IF"; "BRANCH"; "CONDITION"; "BRANCHED"; "BRANCHING"; "BRANCHES";
                     "CONDITIONS"; "CONDITIONAL"; "CONDITIONALS"; "CONDITIONALLY"];
            def = noop; }
          { words = [];
            def = noop; };
    ]

  let starting_word = "START"

end

module Dict = Dictionary.Dict(Words)
