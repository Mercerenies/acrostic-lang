
type t =
  | Standard_run of { filename: string;
                      debug_level: Debugger.t }
  | Gen_docs

let usage = "Usage: ./acrostic.native <filename>"

let parse_args argv =
  let open Printf in
  try
    let filename = ref None in
    let debug_level = ref 0 in
    let gen_docs = ref false in
    let set_filename s =
      match filename.contents with
      | Some _ -> raise (Arg.Bad "Only one filename expected")
      | None -> filename.contents <- Some s in
    let spec = [
        "--", Arg.Rest set_filename, "Interpret the remaining argument as a filename";
        "-d", Arg.Set_int debug_level, "Set debug level (0 = No debug; 1 = Print stack; 2 = Print everything)";
        "-D", Arg.Set gen_docs, "Generate documentation and then exit";
      ] in
    let error s = raise (Arg.Bad (sprintf "%s: %s\n%s" argv.(0) s (Arg.usage_string spec usage))) in
    Arg.parse_argv argv spec set_filename usage;
    if gen_docs.contents then
      Gen_docs
    else
      match filename.contents with
      | None -> error "Filename expected."
      | Some f ->
         match Debugger.of_int debug_level.contents with
         | None -> error "Invalid debug level."
         | Some d -> Standard_run { filename = f; debug_level = d; }
  with
  | Arg.Bad(s) ->
    prerr_endline s;
    exit 1
  | Arg.Help(s) ->
     prerr_endline s;
     exit 0
