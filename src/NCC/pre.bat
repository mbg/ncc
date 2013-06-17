@echo off
alex Cada\Lexer.x -o Cada\Lexer.hs
happy Cada\Parser.y -o Cada\Parser.hs --info=happy.log --ghc -c -a