
type t =
  { filename: string;
    debug_level: Debugger.t }

val parse_args : string array -> t
