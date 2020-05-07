
open Batteries
open Printf

type doc =
  { word: string;
    synonyms: string list;
    antonyms: string list;
    text: string; }

module Gen(W : Dictionary.WordList) = struct

  module Dict = Dictionary.Dict(W)

  let doc_for_entry w def =
    { word = w;
      synonyms = Dict.synonyms_of w;
      antonyms = Dict.antonyms_of w;
      text = def.Dictionary.doc; }

  let markdown_docs =
    let open Dictionary in
    let expand_def d = List.map (fun w -> w, d) d.words in
    W.entries |>
      List.map (fun e -> expand_def e.forward @ expand_def e.backward) |>
      List.concat |>
      List.map (fun (w, d) -> doc_for_entry w d)

end

let string doc =
  let syn =
    if List.is_empty doc.synonyms then
      ""
    else
      sprintf "*Synonyms: %s*\n\n" (String.concat ", " doc.synonyms) in
  let ant =
    if List.is_empty doc.antonyms then
      ""
    else
      sprintf "*Antonyms: %s*\n\n" (String.concat ", " doc.antonyms) in
  sprintf "## %s\n\n%s%s%s"
    doc.word syn ant doc.text

let string_of_list lst =
  let compare x y = String.compare x.word y.word in
  lst |>
    List.sort compare |>
    List.map string |>
    String.concat "\n\n"
