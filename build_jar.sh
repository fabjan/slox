#! /bin/sh

set -eu
set -x

mill slox.assembly
cp out/slox/assembly.dest/out.jar out/slox.jar

set +x

echo
echo "build complete, output is in out/slox.jar"
echo "run with e.g:"
echo "	java -jar out/slox.jar"
