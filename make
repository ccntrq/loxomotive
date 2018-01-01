#!/bin/bash
ghc --make -odir .build/ -hidir .build/ -main-is Lox src/Scanner.hs src/Token.hs src/TokenType.hs Lox.hs
