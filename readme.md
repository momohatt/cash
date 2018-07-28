# cash

## Description

* a tiny shell implemented in OCaml
* available built-in commands: `exit`, `history`, `cd`, `jobs`, `fg`, `bg`

## Requirements

* [ocaml-linenoise](https://github.com/fxfactorial/ocaml-linenoise)
    * support GNU readline functionality in OCaml
    * install with `opam install linenoise`

* [extunix](https://github.com/ygrek/extunix)
    * extension of OCaml Unix module ([API documentation](http://ygrek.org.ua/p/ocaml-extunix/api/ExtUnixSpecific.html))
    * install with `opam install extunix`

## Usage

    $ make
    $ ./main


