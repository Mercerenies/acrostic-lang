
type doc =
  { word: string;
    synonyms: string list;
    antonyms: string list;
    text: string; }

module Gen(W : Dictionary.WordList) : sig

  val markdown_docs : doc list

end

val string : doc -> string

val string_of_list : doc list -> string
