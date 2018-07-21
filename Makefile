SOURCES = syntax.ml lexer.mll parser.mly main.ml
RESULT  = main

YFLAGS = -v

all: byte-code byte-code-library

-include OCamlMakefile
