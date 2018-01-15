#!/bin/bash 
ghc --make -Wall -dynamic -odir .build/ -hidir .build/ -main-is Loxomotive src/*.hs Loxomotive.hs -o loxomotive
