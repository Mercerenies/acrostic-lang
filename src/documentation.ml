
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
      synonyms = List.sort String.compare (Dict.synonyms_of w);
      antonyms = List.sort String.compare (Dict.antonyms_of w);
      text = def.Dictionary.doc; }

  let markdown_docs =
    let open Dictionary in
    let expand_def d = List.map (fun w -> w, d) d.words in
    W.entries |>
      List.map (fun e -> expand_def e.forward @ expand_def e.backward) |>
      List.concat |>
      List.map (fun (w, d) -> doc_for_entry w d)

end

let linked_word w =
  sprintf "[%s](#%s)" w (String.lowercase w)

let string doc =
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

let string_of_list lst =
  let compare x y = String.compare x.word y.word in
  lst |>
    List.sort_uniq compare |>
    List.map string |>
    String.concat "\n\n"
