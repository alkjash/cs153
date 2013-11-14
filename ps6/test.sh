#! /bin/bash

ocaml scish_ast.cmo scish_eval.cmo mlish_ast.cmo mlish_type_check.cmo mlish_compile.cmo ml_lex.cmo ml_parse.cmo monadic.cmo test.cmo
