
open Batteries

module Eval = Evaluator.Eval(StandardDictionary.Dict)

let (>>=) = Result.Monad.(>>=)

let () =
  let { CmdArgs.filename=filename; CmdArgs.debug_level=debug } = CmdArgs.parse_args Sys.argv in
  let grid = CodeGrid.map Char.uppercase @@ InputFile.load_file filename in
  let result = Eval.check_all_words_exist grid >>= fun () -> Eval.execute_code debug grid in
  match result with
  | Error e -> print_endline (Evaluator.err_to_string e)
  | Ok state -> begin
      flush stdout;
      Debugger.print_stack "Value" state.stack debug;
      Debugger.print_stack "Storage" state.storage debug
    end
