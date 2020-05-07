
open Batteries

module Eval = Evaluator.Eval(StandardDictionary.Dict)

let (>>=) = Result.Monad.(>>=)

let () =
  let open Printf in
  let { CmdArgs.filename=filename; } = CmdArgs.parse_args Sys.argv in
  let grid = CodeGrid.map Char.uppercase @@ InputFile.load_file filename in
  let result = Eval.check_all_words_exist(grid) >>= fun () -> Eval.execute_code(grid) in
  match result with
  | Error e -> print_endline (Evaluator.err_to_string e)
  | Ok state -> begin
      (* Probably won't actually print the stack like this. This is
           just for debugging. *)
      flush stdout;
    (*
        print_endline "Final stack:\n";
        List.iter (fun i -> printf "  %i\n" i) state.stack
     *)
    end
