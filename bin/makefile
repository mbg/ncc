all: prelude tests

NCC = ./ncc
LIB_SOURCES = ../lib/Cada/Class/Default.cada ../lib/Cada/Class/Eq.cada ../lib/Cada/Class/Ord.cada ../lib/Cada/Class/Num.cada ../lib/Cada/Class/Show.cada ../lib/Cada/Class/Functor.cada ../lib/Cada/Class/Monad.cada ../lib/Cada/Class/MonadTrans.cada ../lib/Cada/Class/Monoid.cada ../lib/Cada/Bool.cada ../lib/Cada/Either.cada ../lib/Cada/IO.cada ../lib/Cada/List.cada ../lib/Cada/Maybe.cada ../lib/Cada/Pair.cada ../lib/Cada/Monad/Identity.cada ../lib/Cada/Monad/StateT.cada ../lib/Cada/Monad/WriterT.cada ../lib/Cada.cada
NCC_OPTIONS = --path ../lib/ --keep-hs

prelude: ../lib/NCC/Internal.hs
	ghc -c -O2 ../lib/NCC/Internal.hs -o ../lib/NCC/Internal.o
	$(NCC) --no-link --no-implicit-prelude $(LIB_SOURCES)

tests: fib compiler car vm

fib: ../regression/Fibonacci.cada
	$(NCC) ../regression/Fibonacci.cada -o fib --entry Fibonacci --path  ../lib/ --trace

compiler: ../regression/MonadicCompiler.cada
	$(NCC) ../regression/MonadicCompiler.cada -o afpcwk2 --entry MonadicCompiler --path  ../lib/

car: ../regression/CarTest.cada
	$(NCC) ../regression/CarTest.cada -o car --entry CarTest --path ../lib/ --keep-hs --trace

vm: ../regression/VM.cada
	$(NCC) ../regression/VM.cada -o vm --entry VM $(NCC_OPTIONS)

