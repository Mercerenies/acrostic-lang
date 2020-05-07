
type doc =
  { word: string;
    synonyms: string list;
    antonyms: string list;
    text: string; }

val string : doc -> string

val string_of_list : doc list -> string

val full_docs : string -> doc list -> string

module Gen(W : Dictionary.WordList) : sig

  val markdown_docs : doc list

end
