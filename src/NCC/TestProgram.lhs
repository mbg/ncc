> import System.Environment
> import System.IO

> import Compiler.Core

> data Test = Test [FilePath] [String]

> preludeTest :: Test
> preludeTest = Test [
>   "./../../lib/Cada/Class/Eq.cada",
>   "./../../lib/Cada/Class/Ord.cada",
>   "./../../lib/Cada/Class/Num.cada",
>   "./../../lib/Cada/Class/Show.cada",
>   "./../../lib/Cada/Class/Functor.cada",
>   "./../../lib/Cada/Class/Monoid.cada",
>   "./../../lib/Cada/Class/Monad.cada",
>   "./../../lib/Cada/Class/MonadTrans.cada",
>   "./../../lib/Cada/Class/Collection.cada",
>   "./../../lib/Cada/Bool.cada",
>   "./../../lib/Cada/Maybe.cada",
>   "./../../lib/Cada/Either.cada",
>   "./../../lib/Cada/List.cada",
>   "./../../lib/Cada/Pair.cada",
>   "./../../lib/Cada/IO.cada",
>   "./../../lib/Cada/Monad/Identity.cada",
>   "./../../lib/Cada/Monad/WriterT.cada",
>   "./../../lib/Cada/Monad/StateT.cada",
>   "./../../lib/Cada.cada"] ["--no-link", "--no-implicit-prelude"]

> hwTest :: Test
> hwTest = Test ["./../../regression/HelloWorld.cada"] ["--entry", "HelloWorld"]

> labelTest :: Test
> labelTest = Test ["./../../regression/LabelTree.cada"] ["--entry", "LabelTree"]

> compilerTest :: Test 
> compilerTest = Test [
>   "./../../regression/MonadicCompiler.cada"] ["--entry", "MonadicCompiler", "-o", "afpcwk2.exe"]

> letTest :: Test
> letTest = Test ["./../../regression/syntax/let.cada"] ["--entry", "Let", "-o", "let.exe"]

> aliasTest :: Test
> aliasTest = Test ["./../../regression/syntax/alias.cada"] ["--no-link"]

> huttonTest :: Test
> huttonTest = Test ["./../../regression/hutton.cada"] ["--entry", "Hutton", "-o", "hutton.exe"]

> fibTest :: Test
> fibTest = Test ["./../../regression/Fibonacci.cada"] ["--entry", "Fibonacci", "-o", "fib.exe"]


> minimalTest :: Test
> minimalTest = Test ["./../../regression/minimal.cada"] []

> depTest :: Test
> depTest = Test ["./../../regression/mutdep1.cada", "./../../regression/mutdep2.cada"] []

> dataTest :: FilePath
> dataTest = "./../../regression/data.cada"

> typeClassTest :: FilePath
> typeClassTest = "./../../regression/typeclass.cada"

> stateTest :: Test
> stateTest = Test ["./../../regression/state.cada"] []

> funTest :: FilePath
> funTest = "./../../regression/functions.cada"

> dataTypesTest :: FilePath
> dataTypesTest = "./../../regression/data-types.cada"

> lambdaTest :: Test
> lambdaTest = Test ["./../../lib/lambda.cada"] []

> stackTest :: Test
> stackTest = Test ["./../../lib/stack.cada"] []

 allTests :: [Test]
 allTests = [minimalTest, dataTest, typeClassTest, funTest, dataTypesTest]

> testConditions :: [String]
> testConditions = ["--trace", {-"--use-stdout",-} "--keep-hs", "--path", "./../../lib/"]

> runTest :: Test -> IO ()
> runTest t = runTestWith t []

> runTestWith :: Test -> [String] -> IO ()
> runTestWith (Test xs ys) as = do
>   withArgs (xs ++ ys ++ as ++ testConditions) runCompiler
>   putStrLn "Test completed."

> parseFile :: Test -> IO ()
> parseFile t = runTestWith t ["--stop-after-parser"]
