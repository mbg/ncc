{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.EnvMerge (
>   mergeEnvs
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Applicative
>   import Control.Monad
    
>   import Data.Foldable (foldrM)
>   import qualified Data.Map as M
    
>   import TypeSystem.Instance
>   import TypeSystem.Environments
>   import TypeSystem.Unify
>   import TypeSystem.Tags

>   import Utility.Accum
>   import Utility.Map

    {----------------------------------------------------------------------}
    {-- Merging Environments                                              -}
    {----------------------------------------------------------------------}

>   type Merge a   = Either (Accum ShowS) (Env a)
>   type Handler a = String -> a -> a -> Env a -> Merge a
    
>   mergeCheck :: Handler a -> String -> a -> Env a -> Merge a
>   mergeCheck f k x m = case M.lookup k m of
>       (Just y) -> f k x y m
>       Nothing  -> return $ M.insert k x m

>   yoloIt :: Handler a
>   yoloIt k x y m = return $ M.insert k x m

>   errorIfDefined :: Handler a 
>   errorIfDefined k x y _ = fail $ k ++ " is already defined"

>   checkForOverlap :: Envs -> String -> Instance -> [Instance] -> Either (Accum ShowS) [Instance]
>   checkForOverlap env k is iss = case findOverlapInList env k is iss of
>       Nothing    -> return (is : iss)
>       (Just is') -> fail $ "overlap between"

>   mergeUnlessOverlap :: Envs -> Handler [Instance]
>   mergeUnlessOverlap env k x y m = do 
>       z <- foldrM (checkForOverlap env k) x y 
>       return $ M.insert k z m

>   mergeTags :: Handler Tags
>   mergeTags k x y m = return m
    
>   mergeEnv :: Handler a -> (Envs -> Env a) -> Envs -> Envs -> Merge a
>   mergeEnv h f a b = mapFoldrM (mergeCheck h) (f a) (f b)

    TODO: instance environments will override each other's definitions
    TODO: check for overlapping instances
    
    Merges two environments. 

>   mergeEnvs :: Envs -> Envs -> Either (Accum ShowS) Envs
>   mergeEnvs a b = do 
>       env <- Envs <$> 
>           mergeEnv errorIfDefined adtEnv a b <*>
>           mergeEnv errorIfDefined alEnv a b  <*>
>           mergeEnv errorIfDefined clEnv a b  <*>
>           mergeEnv errorIfDefined exEnv a b  <*>
>           pure M.empty                       <*>
>           mergeEnv errorIfDefined stEnv a b  <*>
>           mergeEnv mergeTags tgEnv a b
>       iss <- mergeEnv (mergeUnlessOverlap env) inEnv a b
>       return $ env { inEnv = iss }

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
