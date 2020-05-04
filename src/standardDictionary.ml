
open Batteries
open Functions

module Words = struct

  open Dictionary

  let entries = [
      { forward = ["START"; "BEGIN"];
        forward_def = (fun t -> Ok t);
        backward = ["END"; "FINISH"];
        backward_def = terminate; };
      { forward = ["ONE"];
        forward_def = (fun t -> Ok t);
        backward = [];
        backward_def = (fun t -> Ok t); };
      { forward = ["TWO"];
        forward_def = (fun t -> Ok t);
        backward = [];
        backward_def = (fun t -> Ok t); };
    ]

  let starting_word = "START"

end

module Dict = Dictionary.Dict(Words)
