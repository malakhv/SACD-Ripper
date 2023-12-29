#!/bin/sh

PASCAL_KIT="./../../../PascalKit/src/util"
SACD="./../sacd/rainbow"

# Clear build (out) dir
rm -R -f ./build
mkdir -p build

# Compile programm
fpc ./src/Program.pas \
     -Fu$PASCAL_KIT \
     -Fu$SACD \
     -FEbuild \
     -osacd
