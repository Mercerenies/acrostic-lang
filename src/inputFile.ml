
open Batteries

let load_file s =
  let lines = List.of_enum (File.lines_of s) in
  CodeGrid.of_list lines
