{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Compiler.Monad (
>   module Control.Monad.State,
>   module Utility.StateM,

>   CompilerState(..),
>   Compiler(..),

>   logPutStr,
>   logPutStrLn,
>   logPutStrLnS,

>   outPutStrLn,
>   outPutStrLnS,

>   logSection,

>   failWith,
>   failWithS,

>   queryCfg,
>   whenCfg,
>   unlessCfg,
>   ifCfg,

>   addPath,
>   evalCompiler
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Prelude hiding (catch)
    
>   import Control.Applicative
>   import Control.Exception
>   import Control.Monad.State

>   import System.Exit

>   import System.FilePath
>   import System.IO
    
>   import Compiler.Configuration
>   import Compiler.Context

>   import Utility.Monad
>   import Utility.PrettyPrint
>   import Utility.StateM

    {----------------------------------------------------------------------}
    {-- Compiler Monad                                                    -}
    {----------------------------------------------------------------------}

>   type Log = Maybe Handle
    
>   data CompilerState = CS {
>       cmpConfig :: Config,
>       cmpLog    :: Log,
>       cmpCtxs   :: [Context],
>       cmpPaths  :: [FilePath]
>   }
    
>   type Compiler = StateMT CompilerState IO

    {----------------------------------------------------------------------}
    {-- Log                                                               -}
    {----------------------------------------------------------------------}
    
>   openLog :: Bool -> Bool -> IO Log
>   openLog False _    = return Nothing
>   openLog True True  = return (Just stdout)
>   openLog True False = Just <$> openFile "ncc.log" WriteMode

>   closeLog :: Log -> IO ()
>   closeLog = maybeDo hClose

>   withLog :: (Log -> IO ()) -> Compiler ()
>   withLog f = gets cmpLog >>= \l -> liftIO (f l >> maybeDo hFlush l) 

>   logPutStrIO :: String -> Log -> IO ()
>   logPutStrIO xs = maybeDo (flip hPutStr xs)

>   logPutStr :: String -> Compiler ()
>   logPutStr = withLog . logPutStrIO

>   logPutStrLnIO :: String -> Log -> IO ()
>   logPutStrLnIO xs = maybeDo (flip hPutStrLn xs)

>   logPutStrLn :: String -> Compiler ()
>   logPutStrLn = withLog . logPutStrLnIO

>   logPutStrLnS :: ShowS -> Compiler ()
>   logPutStrLnS = logPutStrLn . ppDo

>   outPutStrLn :: String -> Compiler ()
>   outPutStrLn xs = do
>       liftIO $ putStrLn xs
>       b <- queryCfg cfgUseStdout
>       unless b $ logPutStrLn xs

>   outPutStrLnS :: ShowS -> Compiler ()
>   outPutStrLnS = outPutStrLn . ppDo

>   ppSection :: String -> String
>   ppSection xs = ppDo $
>       ppMany (showChar '=') 79 .
>       ppNewLine .
>       showString "== " .
>       showString xs .
>       ppNewLine .
>       ppMany (showChar '=') 79 .
>       ppNewLine

>   logSection :: String -> Compiler ()
>   logSection = logPutStr . ppSection

    {----------------------------------------------------------------------}
    {-- Helpers                                                           -}
    {----------------------------------------------------------------------}

>   failWith :: String -> Compiler a
>   failWith msg = outPutStrLn msg >> liftIO exitFailure

>   failWithS :: ShowS -> Compiler a
>   failWithS = failWith . ppDo
    
    {----------------------------------------------------------------------}
    {-- External Interface                                                -}
    {----------------------------------------------------------------------}

>   queryCfg :: (Config -> a) -> Compiler a
>   queryCfg f = f <$> gets cmpConfig

>   whenCfg :: (Config -> Bool) -> Compiler () -> Compiler ()
>   whenCfg p m = do
>       v <- queryCfg p
>       when v m

>   unlessCfg :: (Config -> Bool) -> Compiler () -> Compiler ()
>   unlessCfg p = whenCfg (not . p)

>   ifCfg :: (Config -> Bool) -> Compiler a -> Compiler a -> Compiler a
>   ifCfg p m m' = queryCfg p >>= \p -> if p then m else m'

>   addPath :: FilePath -> Compiler ()
>   addPath p = modify $ \s -> s { cmpPaths = p : cmpPaths s }
    
>   cleanupH :: Log -> Config -> IO ()
>   cleanupH l cfg = do
>       unless (cfgUseStdout cfg) (closeLog l)
    
>   installH :: IO (Maybe a) -> IO () -> IO (Maybe a)
>   installH m h = m `finally` h
    
>   evalCompiler :: Compiler a -> Config -> IO (Maybe a)
>   evalCompiler m cfg = do
>       l <- openLog (cfgTrace cfg) (cfgUseStdout cfg)
>       evalStateMT m (CS cfg l [] ["."]) `finally` cleanupH l cfg
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
