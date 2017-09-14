#!/bin/bash

for f in `ls *.lisp`;
  do
    echo $f
    ./$f $f;
done

