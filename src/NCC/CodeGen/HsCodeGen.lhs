{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module CodeGen.HsCodeGen (
>   generateEntryHs,
>   generateHs
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Monad.State

>   import qualified Data.Map as M
    
>   import System.Directory
>   import System.FilePath
>   import System.IO
>   import System.IO.Temp

>   import Cada.AST
>   import Cada.Dependencies

>   import TypeSystem.Types
>   import TypeSystem.PolyType
>   import TypeSystem.DataType
>   import TypeSystem.Alias
>   import TypeSystem.TypeClass
>   import TypeSystem.Instance
>   import TypeSystem.BindGroup
>   import TypeSystem.Environments

>   import Compiler.Monad

>   import CodeGen.HsPretty
    
>   import Utility.PrettyPrint

    {----------------------------------------------------------------------}
    {-- HsCodeGen Monad                                                   -}
    {----------------------------------------------------------------------}

>   type HsCodeGen = StateT Handle IO

>   writeHs :: ShowS -> HsCodeGen ()
>   writeHs f = get >>= \h -> liftIO $ hPutStrLn h (ppDo f)
    
    {----------------------------------------------------------------------}
    {-- Header                                                            -}
    {----------------------------------------------------------------------}

>   generateHeader :: String -> [String] -> Envs -> BindGroup -> HsCodeGen ()
>   generateHeader n is envs bg = do
>       writeHs $ hsLanguageExt "NoImplicitPrelude"
>       --writeHs $ hsLanguageExt "GADTSyntax"
>       writeHs $ hsLanguageExt "RebindableSyntax"
>       writeHs $ hsModule n is (map explName $ bgExpls bg) envs
>       mapM_ (writeHs . hsImport) is

    {----------------------------------------------------------------------}
    {-- Types                                                             -}
    {----------------------------------------------------------------------}
    
    Pretty-prints a data constructor (if GADTSyntax is available).
    
   generateCtr :: (String, PolyType) -> HsCodeGen ()
   generateCtr (n, pt) = do
       writeHs $ hsDataConstr n pt
    
    Pretty-prints an ADT.
    
>   generateADT :: (String, ADT) -> HsCodeGen ()
>   generateADT (n, ADT k as) = do
>       writeHs $ hsDataHeader n k
>       -- use generateCtr if GADTSyntax is available
>       --mapM_ generateCtr $ M.toList as
>       -- otherwise use hsDataConstrs
>       writeHs $ hsDataConstrs $ M.toList as
    
>   generateAlias :: (String, Alias) -> HsCodeGen ()
>   generateAlias (n, Alias k a pt) = do
>       writeHs $ hsTypeAlias n k a pt

>   generateClass :: (String, TypeClass) -> HsCodeGen ()
>   generateClass (n, TypeClass ctx k as) = do
>       writeHs $ hsClassHeader n ctx k 
>       mapM_ (writeHs . uncurry (hsTypeDecl 1)) $ M.toList as
    
>   generateInst :: String -> Instance -> HsCodeGen ()
>   generateInst n (Inst (ctx :=> mt) eqs) = do
>       writeHs $ hsInstHeader n ctx mt
>       mapM_ (writeHs . hsEquation 1) eqs
    
>   generateInsts :: (String, [Instance]) -> HsCodeGen ()
>   generateInsts (n, iss) = do
>       mapM_ (generateInst n) iss
    
>   generateTypes :: Envs -> HsCodeGen ()
>   generateTypes envs = do
>       mapM_ generateADT $ M.toList $ adtEnv envs
>       mapM_ generateAlias $ M.toList $ alEnv envs
>       mapM_ generateClass $ M.toList $ clEnv envs
>       mapM_ generateInsts $ M.toList $ inEnv envs
    
    {----------------------------------------------------------------------}
    {-- Value Definitions                                                 -}
    {----------------------------------------------------------------------}
    
>   generateEq :: Equation -> HsCodeGen ()
>   generateEq = writeHs . hsEquation 0
    
>   generateExpl :: Expl -> HsCodeGen ()
>   generateExpl (Expl n pt es) = do
>       writeHs $ hsTypeDecl 0 n pt
>       mapM_ generateEq es
    
>   generateDecls :: BindGroup -> HsCodeGen ()
>   generateDecls (BG ex iss ins) = do
>       mapM_ generateExpl ex
    
    {----------------------------------------------------------------------}
    {-- External Interface                                                -}
    {----------------------------------------------------------------------}

>   generateEntry :: String -> HsCodeGen ()
>   generateEntry m = do
>       writeHs $ hsLanguageExt "NoImplicitPrelude"
>       writeHs $ showString "module Main where"
>       writeHs $ hsQualImport m "M"
>       writeHs $ showString "main = M.main"
    
>   generateEntryHs :: String -> Compiler FilePath
>   generateEntryHs m = liftIO $ do
>       tmp <- getTemporaryDirectory 
>       dir <- createTempDirectory tmp "NCC" 
>       let
>           hs = dir </> "Main.hs"
>       withFile hs WriteMode $ evalStateT (generateEntry m)
>       return hs
    
>   generateHs :: ModuleNode -> FilePath -> Envs -> BindGroup -> Compiler ()
>   generateHs (ctx,n,ds) fp envs bg = liftIO $ withFile fp WriteMode $ evalStateT $ do
>       generateHeader n ds envs bg
>       generateTypes envs
>       generateDecls bg
>       return ()
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
