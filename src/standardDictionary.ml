
open Batteries
open Functions
open Dictionary
open Printf

let literal_number_doc n =
  sprintf
    "Pushes the literal number %d onto the value stack. If executed \
     backwards, pushes the literal number %d onto the stack."
    n (-n)

let literal_number xs n =
  { forward =
      { words = xs;
        def = push_stack n;
        doc = literal_number_doc n; };
    backward =
        { words = [];
          def = push_stack (- n);
          doc = ""; };
    forward_branch = no_branch;
    backward_branch = no_branch; }

let entry f b =
  { forward = f;
    backward = b;
    forward_branch = no_branch;
    backward_branch = no_branch; }

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
          def = (fun t -> Ok t);
          doc = "Starting word. This marks the entrypoint of the \
                 program and, when executed backward, terminates the \
                 program."; }
        { words = ["END"; "FINISH"; "ENDING"; "FINISHING"; "ENDS"; "FINISHES"; "ENDED";
                   "FINISHED"];
          def = terminate;
          doc = "Finishing word. This marks the entrypoint of the \
                 program (backwards) and, when executed forward, \
                 terminates the program."; };
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
          def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_ascii);
          doc = "Sets the IO flag to \"character\" mode. All \
                 subsequent input and output operations will operate \
                 on characters."; }
        { words = ["NUMBER"; "NUMERICALLY"; "INTEGER"; "NUMBERS"; "INTEGERS"; "NUMERICAL"];
          def = successfully (Evaluator.set_flag Flags.IOMode Flags.io_mode_number);
          doc = "Sets the IO flag to \"number\" mode. All subsequent \
                 input and output operations will operate on integers."; };
      entry
        { words = ["PRINT"; "OUTPUT"; "WRITE"; "PRINTS"; "PRINTING"; "PRINTED"; "OUTPUTS";
                   "OUTPUTTING"; "OUTPUTTED"; "WRITES"; "WROTE"; "WRITING"; "DISPLAY"; "DISPLAYS";
                   "DISPLAYING"; "DISPLAYED"];
          def = user_output;
          doc = "Pops the top of the value stack and prints it. If the \
                 IO flag is in character mode, a single character is \
                 printed, whose ASCII value is the top value on the \
                 stack. If the IO flag is in number mode, the value of \
                 the top of the stack is printed numerically in base \
                 10."; }
        { words = ["SCAN"; "INPUT"; "READ"; "SCANNED"; "SCANNING"; "SCANS"; "INPUTTED"; "INPUTTING";
                   "INPUTS"; "READS"; "READING"];
          def = user_input;
          doc = "Reads input from the user and pushes it to the top of \
                 the value stack. If the IO flag is in character mode, \
                 a single character is read, and its ASCII value is \
                 pushed. If the IO flag is in number mode, a sequence \
                 of digits is read and their numerical value (in base \
                 10) is pushed."; };
      entry
        { words = ["ADD"; "ADDITION"; "SUM"; "COMBINE"; "COMBINED"; "ADDING"; "ADDED"; "ADDITIVE";
                   "COMBINING"; "SUMMING"; "SUMMED"; "ADDS"; "COMBINES"; "SUMS"];
          def = binary_op (+);
          doc = "Pops two values, adds them together, and pushes the \
                 result."; }
        { words = ["SUBTRACT"; "SUBTRACTION"; "DIFFERENCE"; "WITHOUT"; "SUBTRACTING"; "SUBTRACTED";
                   "SUBTRACTS"; "DIFFER"; "DIFFERING"; "DIFFERS"; "DIFFERED"];
          def = binary_op (-);
          doc = "Pops two values, subtracts the first value popped \
                 from the second, and pushes the result."; };
      entry
        { words = ["MULTIPLY"; "MULTIPLYING"; "MULTIPLIED"; "MULTIPLIES"; "TIMES"; "OF"];
          def = binary_op ( * );
          doc = "Pops two values, multiplies them together, and pushes \
                 the result."; }
        { words = ["DIVIDE"; "DIVIDING"; "DIVIDED"; "DIVIDES"; "QUOTIENT"; "MODULO"; "BY";
                   "REMAINDER"; "REMAINDERS"; "DIVISION"; "DIVISIONS"];
          def = safe_div;
          doc = "Pops two values and divides them. The top value is \
                 the divisor and the next value is the dividend. \
                 First, the quotient (truncated toward zero) is \
                 pushed, then the remainder of division is pushed onto \
                 the stack. In the case of division by zero, the \
                 program terminates with an error immediately."; };
      self_opposite
        { words = ["NOTHING"; "VOID"; "NULL"; "EMPTY"; "WAIT"; "STANDBY"; "REST";
                   "NOTHINGNESS"; "WAITING"; "RESTING"; "EMPTINESS"; "VOIDS"; "NULLS";
                   "EMPTINESS"; "WAITS"; "WAITED"; "RESTED"; "RESTS"; "NIL"; "NILS";
                   "VOIDED"; "VOIDING"];
          def = noop;
          doc = "This instruction has no effect, whether executed \
                 forward or backward."; };
      entry
        { words = ["DUPLICATE"; "DITTO"; "AGAIN"; "CLONE"; "COPY"; "DUPLICATING"; "DUPLICATED";
                   "DUPLICATES"; "CLONING"; "CLONED"; "CLONES"; "COPYING"; "COPIED"; "COPIES"];
          def = dup;
          doc = "Pops one value off the stack and pushes it twice, \
                 effectively duplicating the value."; }
        { words = ["POP"; "REMOVE"; "POPPING"; "POPS"; "POPPED"; "REMOVING"; "REMOVES"; "REMOVED";
                   "DELETE"; "DELETING"; "DELETES"; "DELETED"; "DISCARD"; "DISCARDING"; "DISCARDED";
                   "DISCARDS"];
          def = (fun state -> Result.map (fun (_, s) -> s) @@ pop_stack state);
          doc = "Pops one value off the stack and discards it."; };
      self_opposite
        { words = ["SWAP"; "SWAPPING"; "SWAPPED"; "SWAPS"; "FLIP"; "FLIPPING";
                   "FLIPS"; "FLIPPED"; "SWITCH"; "SWITCHED"; "SWITCHING"; "SWITCHES";
                   "EXCHANGE"; "EXCHANGES"; "EXCHANGING"; "EXCHANGED"];
          def = swap;
          doc = "Swaps the top two values on the value stack."; };
      entry
        { words = ["STORE"; "PUT"; "STORES"; "STORING"; "STORED"; "PUTS"; "PUTTING";
                   "HOARD"; "HOARDS"; "HOARDING"; "HOARDED"; "KEEP"; "KEEPS"; "KEEPING";
                   "KEPT"; "STASH"; "STASHED"; "STASHING"; "STASHES"; "DEPOSIT"; "DEPOSITING";
                   "DEPOSITED"; "DEPOSITS"];
          def = store_value;
          doc = "Pops one value off the value stack and pushes it onto \
                 the storage stack."; }
        { words = ["RETRIEVE"; "RETRIEVES"; "RETRIEVED"; "RETRIEVING"; "GET"; "GETS"; "GETTING";
                   "GOT"; "WITHDRAW"; "WITHDRAWING"; "WITHDRAWS"; "WITHDREW"; "WITHDRAWAL";
                   "WITHDRAWALS"; "RECALL"; "RECALLS"; "RECALLING"; "RECALLED"; "RECLAIM";
                   "RECLAIMS"; "RECLAIMING"; "RECLAIMED"; "FETCH"; "FETCHED"; "FETCHING";
                   "FETCHES"];
          def = retrieve_value;
          doc = "Pops one value off the storage stack and pushes it \
                 onto the value stack."; };
      branch_top ((<>) 0) ((=) 0) @@
        entry
          { words = ["BRANCH"; "IF"; "CONDITION"; "BRANCHED"; "BRANCHING"; "BRANCHES";
                     "CONDITIONS"; "CONDITIONAL"; "CONDITIONALS"; "CONDITIONALLY"];
            def = noop;
            doc = "This instruction is a no-op when executed. However, \
                   during backtracking, this word will be ignored if \
                   the top value of the stack is zero. If used \
                   backwards, this word will be ignored if the top \
                   value of the stack is nonzero. If the stack is \
                   empty, a branch will never be taken, either forward \
                   or backward."; }
          { words = [];
            def = noop;
            doc = ""; };
    ]

  let starting_word = "START"

end

module Dict = Dictionary.Dict(Words)
