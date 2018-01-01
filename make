#!/bin/bash
ghc --make -dynamic -odir .build/ -hidir .build/ -main-is Lox src/*.hs Lox.hs
