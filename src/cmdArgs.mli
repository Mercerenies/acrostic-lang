
type t =
  | Standard_run of { filename: string;
                      debug_level: Debugger.t }
  | Gen_docs

val parse_args : string array -> t
