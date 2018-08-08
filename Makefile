PACKS = linenoise extunix
SOURCES = syntax.ml lexer.mll parser.mly utils.ml main.ml
RESULT  = main

YFLAGS = -v

all: byte-code byte-code-library

-include OCamlMakefile
