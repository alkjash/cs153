#!/bin/bash
FILES=test/*.fish
for f in $FILES
do
	echo "Processing $f file..."
	echo "Combinator:"
	./ps1comb $f
	echo "YACC:"
	./ps1yacc $f
done
	
