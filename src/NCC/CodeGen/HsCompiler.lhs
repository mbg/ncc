{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module CodeGen.HsCompiler (
>   checkGHC,
>   cmpObjectFile,
>   cmpBinary
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import Control.Applicative

>   import Data.List (intersperse)
    
>   import System.Exit
>   import System.FilePath
>   import System.Process
    
>   import Compiler.Monad

>   import Utility.IO
    
>   ghcProc :: [String] -> IO ProcessHandle
>   ghcProc ps = do
>       (_,_,_,h) <- createProcess $ proc "ghc" ps
>       return h
    
>   ghcVersion :: Compiler String
>   ghcVersion = liftIO $ readProcess "ghc" ["--numeric-version"] ""
    
    {----------------------------------------------------------------------}
    {-- External Interface                                                -}
    {----------------------------------------------------------------------}
    
>   checkGHC :: Compiler ()
>   checkGHC = do
>       v <- ghcVersion
>       logPutStrLn $ "Compiling using GHC version " ++ v    

>   invokeGHC :: [String] -> a -> Compiler a
>   invokeGHC ps x = do
>       is <- concat . intersperse ":" <$> gets cmpPaths
>       logPutStrLn $ "ghc " ++ (concat $ intersperse " " $ ps ++ ["-i" ++ is])
>       r  <- liftIO $ do
>           h <- ghcProc (ps ++ ["-i" ++ is])
>           waitForProcess h
>       case r of
>           ExitSuccess   -> return x
>           ExitFailure n -> failWith $ 
>               "\nCode generation failed: exit code " ++ show n
    
>   cmpObjectFile :: FilePath -> Compiler FilePath
>   cmpObjectFile fp = do
>       let
>           obj = swapExtension ".o" fp    
>       invokeGHC [fp, "-c", "-o", obj] obj

>   cmpBinary :: [FilePath] -> FilePath -> Compiler ()
>   cmpBinary os bf = invokeGHC (os ++ ["-o", bf]) ()

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
