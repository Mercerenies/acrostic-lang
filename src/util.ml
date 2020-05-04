
let pad_left n c s =
  String.make (max 0 (n - String.length s)) c ^ s

let pad_right n c s =
  s ^ String.make (max 0 (n - String.length s)) c
