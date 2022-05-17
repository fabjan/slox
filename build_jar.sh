#! /bin/sh

set -eu
set -x

mill slox.assembly
cp out/slox/assembly.dest/out.jar out/slox.jar
echo "built out/slox.jar"
