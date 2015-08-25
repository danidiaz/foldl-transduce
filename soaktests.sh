#!/bin/bash

mkdir -p dist/build/tests/
cabal exec -- ghc -rtsopts tests/soaktests.hs -o dist/build/tests/soaktests

RESULT=$?
if [ $RESULT -eq 0 ]; then
    echo "Starting soak tests ..." 
    dist/build/tests/soaktests +RTS -M100m -RTS
else
    echo "oops" 
fi
