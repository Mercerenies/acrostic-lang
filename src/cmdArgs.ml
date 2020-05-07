
type t =
  { filename: string }

let usage = "Usage: ./acrostic.native <filename>"

let parse_args argv =
  try
    let filename = ref None in
    let set_filename s =
      match filename.contents with
      | Some _ -> raise (Arg.Bad "Only one filename expected")
      | None -> filename.contents <- Some s in
    let spec = [
        "--", Arg.Rest set_filename, "Interpret the remaining argument as a filename";
      ] in
    Arg.parse_argv argv spec set_filename usage;
    match filename.contents with
    | Some f -> { filename = f; }
    | None -> raise (Arg.Bad (argv.(0) ^ ": Filename expected.\n" ^ Arg.usage_string spec usage))
  with Arg.Bad(s) ->
    prerr_endline s;
    exit 1
