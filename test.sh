#!/bin/bash -ex

ghc --make -O2 -isrc src/Spec.hs
./src/Spec
ghc --make -O2 -isrc bench/ideal-snoc.hs
./bench/ideal-snoc
