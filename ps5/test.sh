#! /bin/bash

for f in ./tests/*.ml
do
  echo "Processing $f"
  ./ps5_mlish $f
done
