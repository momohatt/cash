SOURCES = syntax.ml lexer.mll parser.mly
RESULT  = main

YFLAGS = -v

all: byte-code byte-code-library

-include OCamlMakefile
