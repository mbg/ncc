{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> {-# LANGUAGE BangPatterns #-}
> module Compiler.Interface (
>   objectFileForModule,
>   saveInterface,
>   loadInterface,
>   loadInterfaces
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Debug.Trace (trace)
    
>   import Control.Applicative
    
>   import qualified Data.ByteString.Lazy as BL
>   import qualified Data.Binary as B
>   import Data.Binary.Get
>   import Data.Binary.Put
>   import qualified Data.Map as M

>   import System.Directory
>   import System.IO
>   import System.FilePath

>   import TypeSystem.PolyType
>   import TypeSystem.DataType
>   import TypeSystem.Alias
>   import TypeSystem.TypeClass
>   import TypeSystem.Instance
>   import TypeSystem.Environments
>   import TypeSystem.EnvMerge
    
>   import Compiler.Internals
>   import Compiler.Monad
>   import Compiler.Binary

>   import Utility.IO

    {----------------------------------------------------------------------}
    {-- Module Interfaces                                                 -}
    {----------------------------------------------------------------------}
    
>   type ModuleName = String
    
    {----------------------------------------------------------------------}
    {-- Saving Interfaces                                                 -}
    {----------------------------------------------------------------------} 
    
>   serialiseADT :: String -> ADT -> Put
>   serialiseADT n adt = do
>       B.put n  
>       B.put adt    
    
>   serialiseADTs :: ADTEnv -> Put
>   serialiseADTs adts = do
>       B.put (M.size adts)
>       mapM_ (uncurry serialiseADT) (M.toList adts)

>   serialiseClass :: String -> TypeClass -> Put
>   serialiseClass n cls = do
>       B.put n  
>       B.put cls

>   serialiseClasses :: ClEnv -> Put
>   serialiseClasses cls = do
>       B.put (M.size cls)
>       mapM_ (uncurry serialiseClass) (M.toList cls)

>   serialiseAlias :: String -> Alias -> Put
>   serialiseAlias n als = do
>       B.put n  
>       B.put als

>   serialiseAliases :: AlEnv -> Put
>   serialiseAliases als = do
>       B.put (M.size als)
>       mapM_ (uncurry serialiseAlias) (M.toList als)

>   serialiseExpl :: String -> PolyType -> Put
>   serialiseExpl n ex = do
>       B.put n
>       B.put ex

>   serialiseExpls :: ExEnv -> Put
>   serialiseExpls expls = do
>       B.put (M.size expls)
>       mapM_ (uncurry serialiseExpl) (M.toList expls)
    
>   serialiseInst :: String -> [Instance] -> Put
>   serialiseInst n is = do
>       B.put n
>       B.put (length is)
>       mapM_ B.put is

>   serialiseInsts :: InEnv -> Put
>   serialiseInsts insts = do
>       B.put (M.size insts)
>       mapM_ (uncurry serialiseInst) (M.toList insts)
    
>   serialiseEnvs :: ModuleName -> Envs -> Handle -> IO ()
>   serialiseEnvs n e h = BL.hPut h $ runPut $ do
>       B.put n
>       serialiseADTs (adtEnv e)
>       serialiseAliases (alEnv e)
>       serialiseClasses (clEnv e)
>       serialiseExpls (exEnv e)
>       serialiseInsts (inEnv e)
    
>   saveInterface :: String -> ModuleName -> Envs -> Compiler ()
>   saveInterface f n e = liftIO $ withFile f WriteMode (serialiseEnvs n e)
    
    {----------------------------------------------------------------------}
    {-- Loading Interfaces                                                -}
    {----------------------------------------------------------------------}
    
>   deserialiseADT :: Get (String, ADT)
>   deserialiseADT = do
>       n   <- B.get
>       adt <- B.get
>       return (n,adt)
    
>   deserialiseADTs :: Get ADTEnv
>   deserialiseADTs = do
>       c <- B.get
>       M.fromList <$> replicateM c deserialiseADT

>   deserialiseAlias :: Get (String, Alias)
>   deserialiseAlias = do
>       n   <- B.get
>       als <- B.get
>       return (n,als)

>   deserialiseAliases :: Get AlEnv
>   deserialiseAliases = do
>       c <- B.get
>       M.fromList <$> replicateM c deserialiseAlias

>   deserialiseClass :: Get (String, TypeClass)
>   deserialiseClass = do
>       n   <- B.get
>       cls <- B.get
>       return (n,cls)
    
>   deserialiseClasses :: Get ClEnv
>   deserialiseClasses = do
>       c <- B.get
>       M.fromList <$> replicateM c deserialiseClass

>   deserialiseExpl :: Get (String, PolyType)
>   deserialiseExpl = do
>       n  <- B.get
>       ex <- B.get
>       return (n,ex)

>   deserialiseExpls :: Get ExEnv
>   deserialiseExpls = do
>       c <- B.get
>       M.fromList <$> replicateM c deserialiseExpl
    
>   deserialiseInst :: Get (String, [Instance])
>   deserialiseInst = do
>       n  <- B.get
>       c  <- B.get
>       is <- replicateM c B.get
>       return (n,is)   
    
>   deserialiseInsts :: Get InEnv
>   deserialiseInsts = do
>       c <- B.get
>       M.fromList <$> replicateM c deserialiseInst
    
>   loadHeader :: Get Envs
>   loadHeader = do
>       n <- (B.get :: Get String)
>       Envs <$> 
>           deserialiseADTs    <*> 
>           deserialiseAliases <*> 
>           deserialiseClasses <*> 
>           deserialiseExpls   <*>
>           deserialiseInsts 
    
>   deserialiseEnvs :: Handle -> IO Envs
>   deserialiseEnvs h = runGet loadHeader <$> BL.hGetContents h

>   fileNameForModule :: ModuleName -> FilePath
>   fileNameForModule mn = map (\x -> case x of 
>       '.'       -> pathSeparator
>       otherwise -> x) mn <.> ".co"

>   moduleForFileName :: FilePath -> ModuleName
>   moduleForFileName fp = map 
>       (\x -> if x == pathSeparator then '.' else x) 
>       (dropExtension fp)

>   objectFileNameForModule :: ModuleName -> FilePath
>   objectFileNameForModule = swapExtension ".o" . fileNameForModule

>   objectFileForModule :: ModuleName -> Compiler FilePath
>   objectFileForModule mn = do
>       ps <- gets cmpPaths 
>       findFileIn (objectFileNameForModule mn) ps
    
>   findFileIn :: FilePath -> [FilePath] -> Compiler FilePath
>   findFileIn mn []     = failWithS $ 
>       showString "Can't find module: " . showString mn
>   findFileIn mn (p:ps) = do
>       ex <- liftIO $ doesFileExist (p </> mn)
>       if ex then return (p </> mn)
>       else findFileIn mn ps
    
>   loadFrom :: FilePath -> [FilePath] -> Compiler Envs
>   loadFrom mn ps = do
>       fp <- findFileIn mn ps
>       logPutStrLnS $ 
>           showString " Loading interface for " . 
>           showString (moduleForFileName mn)
>       liftIO $ do
>           h <- openFile fp ReadMode 
>           hSetBinaryMode h True
>           !envs <- deserialiseEnvs h
>           hClose h
>           return envs
    
    Loads an interface file for a module.
    
>   loadInterface :: ModuleName -> Compiler Envs
>   loadInterface m = case isInternal m of
>       (Just envs) -> return envs
>       Nothing     -> do
>           ps <- gets cmpPaths
>           loadFrom (fileNameForModule m) ps

>   loadInterfaces :: [ModuleName] -> Compiler Envs
>   loadInterfaces []     = return initialEnvs
>   loadInterfaces (m:ms) = do
>       e  <- loadInterface m
>       es <- loadInterfaces ms
>       case mergeEnvs e es of
>           (Left errs) -> failWithS (head (errs [])) -- TODO
>           (Right env) -> return env
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
