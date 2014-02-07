all: update ncc lib

update: lexer parser

lexer: Cada/Lexer.x 
	alex Cada/Lexer.x -o Cada/Lexer.hs

parser: Cada/Parser.y 
	happy Cada/Parser.y -o Cada/Parser.hs

ncc: NCC.lhs
	ghc NCC.lhs -o ../../bin/ncc

lib: NCC/Internal.hs
	ghc -c NCC/Internal.hs

clean: *.o *.hi
	rm -r *.o
	rm -r *.hi
