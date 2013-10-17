{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Compiler.Core (
>   runCompiler
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Prelude hiding (catch)

>   import Data.List (nub)
    
>   import System.Directory
>   import System.Exit
>   import System.FilePath
>   import System.IO hiding (hGetContents)
>   import System.IO.Strict (hGetContents, run)

>   import Control.Applicative
>   import Control.Exception
    
>   import Compiler.Configuration
>   import Compiler.Context
>   import Compiler.Monad
>   import Compiler.Interface
>   import Compiler.Internals

>   import Cada.AST
>   import Cada.Parser
>   import Cada.PrettyPrint
>   import Cada.Dependencies
>   import Cada.PrettyPrint

>   import TypeSystem.Assump
>   import TypeSystem.STypeDemotion
>   import TypeSystem.KindInference
>   import TypeSystem.Conversion
>   import TypeSystem.Environments
>   import TypeSystem.EnvMerge
>   import TypeSystem.Sort
>   import TypeSystem.PreludeTypes
>   import TypeSystem.TypeInference
>   import TypeSystem.PrettyPrint

>   import Internal.Kinds

>   import CodeGen.HsCompiler
>   import CodeGen.HsCodeGen

>   import Utility.PrettyPrint
>   import Utility.IO

    {----------------------------------------------------------------------}
    {-- Exception Handling                                                -}
    {----------------------------------------------------------------------} 

    Catches program exceptions and reports them to the standard error output.
    
>   exWrapper :: SomeException -> IO a
>   exWrapper ex = do
>       hPutStr stderr "\n\nCompiler panic (the 'impossible' happened):\n\n\t"
>       hPutStrLn stderr (show ex)
>       hPutStrLn stderr "\nThe compiler is now sad and, if it could talk, would offer its sincerest apologies. However, sadly the compiler cannot talk. In the event that the compiler should talk, we urge you to disregard its advice."
>       exitFailure

>   exExit :: ExitCode -> IO a
>   exExit = exitWith
    
>   exHandlers :: [Handler a]
>   exHandlers = [Handler exExit, Handler exWrapper]
    
    {----------------------------------------------------------------------}
    {-- Compiler Initialisation                                           -}
    {----------------------------------------------------------------------} 
    
    Catches program exceptions and reports them to the standard output.
    
>   initCompiler :: IO ()
>   initCompiler = getCfg >>= evalCompiler compiler >> return ()

>   installHandler :: IO ()
>   installHandler = initCompiler `catches` exHandlers
    
>   runCompiler :: IO ()
>   runCompiler = installHandler >> exitSuccess

    {----------------------------------------------------------------------}
    {-- Parser                                                            -}
    {----------------------------------------------------------------------} 

>   runParser :: String -> Compiler (LocP Module)
>   runParser xs = let (ast, ms) = runCadaParser xs in do
>       mapM_ (outPutStrLn . ppDo . ppParserMsg) (unA ms)
>       if hasErrors (ms []) then lift exitFailure
>       else case ast of
>           Nothing  -> lift exitFailure 
>           (Just m) -> return m

    {----------------------------------------------------------------------}
    {-- Compilation Contexts                                              -}
    {----------------------------------------------------------------------} 

>   errNoInputs :: Compiler a
>   errNoInputs = failWith "NCC: No input files."

>   errNameMismatch :: Compiler a
>   errNameMismatch = failWith "Name mismatch"

>   nameCheck :: FilePath -> Module Loc -> Compiler ()
>   nameCheck fp m 
>       | fp == moduleName m = return ()
>       | otherwise          = errNameMismatch
    
>   initCtx :: FilePath -> Compiler Context
>   initCtx fs = do
>       addPath (sourceDir fs)
>       logSection $ "Parsing " ++ fs 
>       xs <- liftIO $ withFile fs ReadMode (\h -> run (hGetContents h))
>       m  <- runParser xs
>       --nameCheck (takeBaseName fs) (unL m) --TODO: consider modules like NCC.Internal!
>       logSection $ "AST for " ++ fs 
>       logPutStrLn (ppAST m)
>       return $ Ctx fs m

>   initCtxs :: Compiler ()
>   initCtxs = do
>       logSection "Parsing input files"
>       fss <- queryCfg cfgInputs
>       when (null fss) errNoInputs
>       mapM_ logPutStrLn fss
>       cts <- mapM initCtx fss
>       modify $ \s -> s { cmpCtxs = cts }

    {----------------------------------------------------------------------}
    {-- Dependency Analysis                                               -}
    {----------------------------------------------------------------------} 

>   ppModuleNode :: ModuleNode -> ShowS
>   ppModuleNode (_,n,_) = ppTab . showString n

>   ppCyclicDep :: [ModuleNode] -> String
>   ppCyclicDep ms = ppDo $
>       showString "Cylic dependencies found in:" .
>       ppNewLine .
>       ppDefsW ppModuleNode ppNewLine ms

>   ppAcyclicDep :: ModuleNode -> String
>   ppAcyclicDep (_,n,_) = ppDo $
>       showString "No cyclic dependencies found in " .
>       ppIdStr n

>   depCheck :: SCC ModuleNode -> Compiler ModuleNode
>   depCheck (CyclicSCC ms) = do 
>       failWith (ppCyclicDep ms)
>   depCheck (AcyclicSCC m) = do
>       logPutStrLn (ppAcyclicDep m)
>       return m

>   addDefaultLibs :: ModuleNode -> Compiler ModuleNode
>   addDefaultLibs (ctx,n,ds) = return (ctx,n,nub (preludeModules ++ ds))

>   depAnalysis :: Compiler [ModuleNode]
>   depAnalysis = do
>       logSection "Dependency analysis"
>       cts <- gets cmpCtxs
>       ms  <- mapM depCheck $ findDeps cts
>       ifCfg cfgNoImplictPrelude 
>           (return ms) 
>           (mapM addDefaultLibs ms)

    {----------------------------------------------------------------------}
    {-- Kind Inference                                                    -}
    {----------------------------------------------------------------------} 

>   demoteTypes :: LocP Module -> Compiler BindGroup
>   demoteTypes m = do
>       return $ demoteModule (unL m)
    
>   runKindInference :: Assumps -> BindGroup -> Compiler Assumps
>   runKindInference as bg = case inferKinds as bg of
>       (Left err) -> failWithS $ ppKindError err
>       (Right rs) -> return rs

    {----------------------------------------------------------------------}
    {-- Type Inference                                                    -}
    {----------------------------------------------------------------------} 

>   convertTypes :: String -> Envs -> Assumps -> Module Loc -> Compiler Envs
>   convertTypes n env as ast = case mkEnvs env as ast of
>       (Left errs)  -> do
>               mapM_ (outPutStrLn . ppLocStr n) errs
>               lift exitFailure
>       (Right envs) -> return envs

>   makeBindGroup :: String -> Envs -> Module Loc -> Compiler BindGroup
>   makeBindGroup n envs ast = do
>       logSection $ "Binding groups for " ++ n
>       bg <- runGrouping envs ast
>       logPutStrLnS $ ppBindGroup bg
>       return bg

>   typecheckModule :: ModuleNode -> Envs -> BindGroup -> Compiler Assumps
>   typecheckModule (ctx,n,ds) envs bg = do
>       logPutStrLnS $ ppEnvs envs

>       logSection $ "Assumptions for " ++ n
>       logPutStrLnS $ ppAssumps 0 (toAssumps envs)
>       logSection $ "Type infererence for " ++ n
>       case inferTypes envs (toAssumps envs) bg of
>           (Left err) -> do
>               failWithS $ ppTypeError err
>           (Right as) -> do
>               logPutStrLnS $ ppAssumps 0 as
>               return as

    {----------------------------------------------------------------------}
    {-- Code Generation                                                   -}
    {----------------------------------------------------------------------}

>   generateCode :: ModuleNode -> Envs -> BindGroup -> Compiler [FilePath]
>   generateCode mn@(ctx,n,ds) envs bg = do
>       logSection $ "Generating Haskell code for " ++ n
>       logPutStrLnS $ ppEnvs envs
>       let
>           hs = swapExtension ".hs" (ifaceName ctx)
>       generateHs mn hs envs bg
>       logSection $ "Compiling " ++ n
>       checkGHC
>       os <- mapM objectFileForModule ds
>       obj <- cmpObjectFile hs
>       unlessCfg cfgKeepHs $ liftIO $ removeFile hs
>       return $ obj : os
    
    {----------------------------------------------------------------------}
    {-- Driver                                                            -}
    {----------------------------------------------------------------------}
    
    envs   : imported type environment
    as     : kinds in the current module
    envs'  : type environment for the current module
    envs'' : all known (explicit) types
    as'    : results of type inference
    
>   processModule :: ModuleNode -> Compiler [FilePath]
>   processModule mn@(ctx,n,ds) = do
>       outPutStrLnS $ 
>           showString "Compiling " . 
>           showString n . 
>           showString "..."
>       logSection $ "Importing modules for " ++ n 
>       envs <- loadInterfaces ds
>       logPutStrLnS $ ppEnvs envs
>       logSection $ "Demoting types in " ++ n
>       bg <- demoteTypes (ctxAST ctx)
>       logPutStrLnS $ ppBindGroup bg
>       logSection $ "Kind inference for " ++ n
>       let
>           ast = unL $ ctxAST ctx
>           kas = toKindAssumps envs
>       as <- runKindInference kas bg
>       logPutStrLnS $ ppAssumps 0 (kas <> kindAssumps <> as)
>       envs' <- convertTypes n envs (kas <> kindAssumps <> as) ast
>       case mergeEnvs envs envs' of
>           (Left err)     -> failWithS $ head $ err []
>           (Right envs'') -> do
>               bg  <- makeBindGroup n envs'' ast
>               as' <- typecheckModule mn envs'' bg
>               logSection $ "Generating interface file for " ++ n
>               logPutStrLnS $ ppEnvs (addAssumps as' envs'')
>               saveInterface (ifaceName ctx) n (addAssumps as' envs')
>               generateCode mn (addAssumps as' envs') (enrichBG as' bg) 
    
>   processEntryPoint :: Compiler FilePath
>   processEntryPoint = do
>       m  <- queryCfg cfgEntry
>       hs <- generateEntryHs m  
>       cmpObjectFile hs

    Links together a bunch of object files using GHC.

>   link :: [FilePath] -> Compiler ()
>   link os = do
>       logSection $ "Compiling"
>       ep <- processEntryPoint
>       out <- queryCfg cfgOutput
>       outPutStrLnS $ 
>           showString "Linking " . 
>           showString out . 
>           showString "..."
>       cmpBinary (ep:os) out    
    
>   compiler :: Compiler Int
>   compiler = do 
>       ps <- queryCfg cfgPaths
>       mapM_ addPath ps
>       initCtxs 
>       whenCfg cfgStopAfterParser $ liftIO exitSuccess
>       ms <- depAnalysis
>       os <- mapM processModule ms
>       unlessCfg cfgNoLinking $ link $ concat os
>       return 0
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
