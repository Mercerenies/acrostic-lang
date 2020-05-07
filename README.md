
# Acrostic

The crossword-based programming language. Acrostic is a language where
your program's code is a two-dimensional mini crossword puzzle.
Writing code is akin to fitting all of the words together.

The interpreter is written in OCaml and requires OCamlbuild and
[Batteries Included](http://batteries.forge.ocamlcore.org/).

Compile with

    ocamlbuild -use-ocamlfind acrostic.native

And run with

    ./acrostic.native <input-file-name>

For details on how to write code in Acrostic, please see the
[wiki](https://github.com/Mercerenies/acrostic-lang/wiki/).
