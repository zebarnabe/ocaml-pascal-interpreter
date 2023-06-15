# ocaml-pascal-interpreter
A Pascal interpreted written in OCaml

Currently only a subset of Pascal language is implemented.
You can check the alex.ml for the implemented language tokens.

This was written in 2005 as an university work for one of the programming courses.
As such evolutions in Pascal or OCaml languages are not integrated in this interpreter.

Honestly I tried to run this recently and was unable to do so, apparently OCaml is no longer what it was in 2005.
A port is required to run this in modern OCaml.

I'll the transcript and translate the README of the original implementation here:

The script load_all.ml includes all the functions, therefor you only need to include this to work with the interpreter.

The functions can be loaded individually, the load_program.ml loads alex.ml and asin.ml.

After the include you can use the following functions:

* alex → Lexical Analyzer: Reads a stream of characters representing the Pascal program.
* asin → Syntatic Analyzer: Reads a stream of tokens returned by alex and returns a rip (representation of the program)
* load_program → Reads a file (filepath as a string) and returns a rip
* run_program → Reads a rip, runs it and at the end shows the memory alterations
* stat_program → Reads a rip and shows the name of all the declared variables and constants

