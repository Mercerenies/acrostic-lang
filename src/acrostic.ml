
open Batteries

module Eval = Evaluator.Eval(StandardDictionary.Dict)

let () =
  let open Printf in
  if Array.length Sys.argv >= 2 then
    let filename = Array.get Sys.argv 1 in
    let grid = CodeGrid.map Char.uppercase @@ InputFile.load_file filename in
    let result = Eval.execute_code(grid) in
    match result with
    | Error e -> print_endline (Evaluator.err_to_string e)
    | Ok state -> begin
        print_endline "Final stack:\n";
        List.iter (fun i -> printf "  %i\n" i) state.stack
      end
  else
    print_endline "Usage: ./acrostic.native <filename>"
