
open Batteries

module Eval = Evaluator.Eval(StandardDictionary.Dict)

module DocGen = Documentation.Gen(StandardDictionary.Words)

let (>>=) = Result.Monad.(>>=)

let () =
  match CmdArgs.parse_args Sys.argv with
  | CmdArgs.Standard_run m ->
     begin
       let filename = m.filename in
       let debug = m.debug_level in
       let grid = CodeGrid.map Char.uppercase @@ InputFile.load_file filename in
       let result = Eval.check_all_words_exist grid >>= fun () -> Eval.execute_code debug grid in
       match result with
       | Error e -> print_endline (Evaluator.err_to_string e)
       | Ok state -> begin
           flush stdout;
           Debugger.print_stack "Value" state.stack debug;
           Debugger.print_stack "Storage" state.storage debug
         end
     end
  | CmdArgs.Gen_docs ->
     let docs = DocGen.markdown_docs in
     print_endline (Documentation.string_of_list docs)
