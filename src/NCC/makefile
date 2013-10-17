all: lexer parser ncc

update: lexer parser

lexer: Cada/Lexer.x 
	alex Cada/Lexer.x -o Cada/Lexer.hs

parser: Cada/Parser.y 
	happy Cada/Parser.y -o Cada/Parser.hs

ncc: NCC.lhs
	ghc -O2 NCC.lhs -o ../../bin/ncc
