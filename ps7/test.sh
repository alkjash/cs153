#! /bin/bash

make

ocaml cish_ast.cmo cish_parse.cmo cish_lex.cmo word32.cmo mips.cmo cfg_ast.cmo cfg.cmo test.cmo b1.cmo
