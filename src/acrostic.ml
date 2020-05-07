
open Batteries

module Eval = Evaluator.Eval(StandardDictionary.Dict)

module DocGen = Documentation.Gen(StandardDictionary.Words)

let (>>=) = Result.Monad.(>>=)

let doc_prologue =
  "# Dictionary

   This is a full list of all of the words available in Latitude, \
   sorted alphabetically. Each word lists its behavior, as well as all \
   synonyms and antonyms for the word.

   There are a few conventions worth noting here.
   * Only a few instructions interact with the storage stack. For this \
   reason, the value stack is often simply referred to as \"the \
   stack\". Unless otherwise noted, the general assumption is that \
   instructions are acting on the value stack.
   * If an instruction attempts to pop arguments off the stack and the \
   stack is empty, that is an error, and the program will terminate \
   immediately with an appropriate error message."

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
     print_endline (Documentation.full_docs doc_prologue docs)
