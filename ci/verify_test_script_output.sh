#! /bin/bash

set -eu

for t in test/test*.lox
do
    echo "$t should print:"
    cat "$t".expect
    echo "..."
    diff "$t".expect <(./out/slox.jar "$t")
    echo "OK!"
    echo
done
