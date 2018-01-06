#!/bin/bash
ghc --make -Wall -dynamic -odir .build/ -hidir .build/ -main-is Lox src/*.hs Lox.hs
