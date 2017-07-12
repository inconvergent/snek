#!/bin/bash

set -eu

for f in `ls *.lisp`;
  do
    ./$f $f;
done

