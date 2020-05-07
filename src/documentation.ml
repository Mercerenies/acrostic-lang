
open Batteries
open Printf

type doc =
  { word: string;
    synonyms: string list;
    antonyms: string list;
    text: string;
    canon: string; }

let linked_word w =
  sprintf "[%s](#%s)" w (String.lowercase w)

let string doc =
  if doc.word == doc.canon then
    let syn =
      if List.is_empty doc.synonyms then
        ""
      else
        sprintf "\n\n*Synonyms: %s*" (doc.synonyms |> List.map linked_word |> String.concat ", ") in
    let ant =
      if List.is_empty doc.antonyms then
        ""
      else
        sprintf "\n\n*Antonyms: %s*" (doc.antonyms |> List.map linked_word |> String.concat ", ") in
    sprintf "## %s\n\n%s%s%s"
      doc.word doc.text syn ant
  else
    sprintf "## %s\n\nSee %s" doc.word (linked_word doc.canon)

let string_of_list lst =
  let compare x y = String.compare x.word y.word in
  lst |>
    List.sort_uniq compare |>
    List.map string |>
    String.concat "\n\n"

let full_docs prologue lst =
  sprintf "%s\n\n%s" prologue (string_of_list lst)

module Gen(W : Dictionary.WordList) = struct

  module Dict = Dictionary.Dict(W)

  let doc_for_entry w def c =
    { word = w;
      synonyms = List.sort String.compare (Dict.synonyms_of w);
      antonyms = List.sort String.compare (Dict.antonyms_of w);
      text = def.Dictionary.doc;
      canon = c; }

  let markdown_docs =
    let open Dictionary in
    let expand_def d = List.map (fun w -> w, d, List.hd d.words) d.words in
    W.entries |>
      List.map (fun e -> expand_def e.forward @ expand_def e.backward) |>
      List.concat |>
      List.map (fun (w, d, c) -> doc_for_entry w d c)

end
