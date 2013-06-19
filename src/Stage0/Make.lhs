{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

Build script for the Nottingham Cada Compiler (NCC).

    {----------------------------------------------------------------------}
    {-- Language extensions                                               -}
    {----------------------------------------------------------------------}

>   {-# LANGUAGE OverloadedStrings #-}
>   {-# LANGUAGE ExtendedDefaultRules #-}

    {----------------------------------------------------------------------}
    {-- Module imports                                                    -}
    {----------------------------------------------------------------------}

>   import Prelude hiding (FilePath)
>   import Shelly

>   import qualified Data.Text.Lazy as TL
    
>   import Config
>   import Monad

>   default (TL.Text)

>   runA :: ShellArg a => FilePath -> [a] -> Sh TL.Text
>   runA bin args = run bin (toTextArg <$> args)

>   runA_ :: ShellArg a => FilePath -> [a] -> Sh ()
>   runA_ bin args = run_ bin (toTextArg <$> args)

    {----------------------------------------------------------------------}
    {-- Pre-processors                                                    -}
    {----------------------------------------------------------------------}
   
>   lexerPath :: FilePath
>   lexerPath = "Cada/Lexer"

>   parserPath :: FilePath
>   parserPath = "Cada/Parser"
    
>   alex :: [FilePath] -> FilePath -> Sh ()
>   alex args out = runA_ "alex" $ "-o" : out : args

>   happyOpts :: [TL.Text]
>   happyOpts = ["--info=happy.log", "--ghc", "-c", "-a", "-o"]

>   happy :: [FilePath] -> FilePath -> Sh ()
>   happy args out = runA_ "happy" $ happyOpts ++ toTextArg out : (toTextArg <$> args)

    {----------------------------------------------------------------------}
    {-- NCC                                                               -}
    {----------------------------------------------------------------------}

>   nccPath :: FilePath
>   nccPath = "src/NCC/ncc.exe"
    
>   makeNCC :: Sh ()
>   makeNCC = do
>       src <- get_env_text "SRC"
>       chdir (fromText src) $ do
>           run_ "ghc" ["-O2", "NCC.lhs", "-o", "ncc.exe"]
>       mv nccPath "ncc.exe"
    
    {----------------------------------------------------------------------}
    {-- Standard library                                                  -}
    {----------------------------------------------------------------------}

>   ghcInternal :: Sh ()
>   ghcInternal = do
>       lib <- get_env_text "LIB"
>       run_ "ghc" ["-c", "lib/NCC/Internal.hs", "-o", "lib/NCC/Internal.o", "-O2"]

>   libs :: [TL.Text]
>   libs = ["lib/Cada/Class/Eq.cada", "lib/Cada/Class/Ord.cada", "lib/Cada/Class/Num.cada", "lib/Cada/Class/Show.cada", "lib/Cada/Class/Functor.cada", "lib/Cada/Class/Monad.cada", "lib/Cada/Class/MonadTrans.cada", "lib/Cada/Class/Monoid.cada", "lib/Cada/Bool.cada", "lib/Cada/Either.cada", "lib/Cada/IO.cada", "lib/Cada/List.cada", "lib/Cada/Maybe.cada", "lib/Cada/Pair.cada", "lib/Cada/Monad/Identity.cada", "lib/Cada/Monad/StateT.cada", "lib/Cada/Monad/WriterT.cada", "lib/Cada.cada"]

>   nccPrelude :: Sh ()
>   nccPrelude = do
>       run_ "ncc" (["--no-link", "--no-implicit-prelude", "--trace"] ++ libs)
    
    {----------------------------------------------------------------------}
    {-- Entry point                                                       -}
    {----------------------------------------------------------------------}

>   main :: IO ()
>   main = do
>       cfg <- getCfg
>       shelly $ verbosely $ do
>           src  <- get_env_text "SRC"
>           alex [src </> lexerPath <.> "x"] (src </> lexerPath <.> "hs")
>           happy [src </> parserPath <.> "y"] (src </> parserPath <.> "hs")
>           makeNCC
>           ghcInternal
>           nccPrelude
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
