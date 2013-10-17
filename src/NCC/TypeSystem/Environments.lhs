{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.Environments (
>   Env,
>   ADTEnv,
>   AlEnv,
>   ClEnv,
>   ExEnv,
>   InEnv,
>   StEnv,
>   Envs(..),
>   initialEnvs,
>   addADT,
>   addAlias,
>   addClass,
>   addExpl,
>   addAssumps,
>   addSt,
>   getExpl,
>   getClsExpl,
>   addInstance,
>   KindIndex,
>   toKindIndex,
>   toKindAssumps,
>   kunion,
>   toAssumps,
>   ppEnvs
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Monad

>   import qualified Data.Map as M

>   import Utility.Errors
>   import Utility.PrettyPrint

>   import TypeSystem.Kind
>   import TypeSystem.PolyType
>   import TypeSystem.KindDemotion
>   import TypeSystem.DataType
>   import TypeSystem.Alias
>   import TypeSystem.TypeClass
>   import TypeSystem.Instance
>   import TypeSystem.Assump
>   import TypeSystem.StateType

    {----------------------------------------------------------------------}
    {-- Environments                                                      -}
    {----------------------------------------------------------------------}
    
    An environment is a mapping of names to types.
    
>   type Env = M.Map String

    We group different things into different environments:

>   type ADTEnv = Env ADT 
>   type AlEnv  = Env Alias
>   type ClEnv  = Env TypeClass
>   type ExEnv  = Env PolyType
>   type InEnv  = Env [Instance]
>   type StEnv  = Env StateType

>   data Envs = Envs {
>       adtEnv :: ADTEnv, -- algebraic data types
>       alEnv  :: AlEnv,  -- type functions
>       clEnv  :: ClEnv,  -- type classes
>       exEnv  :: ExEnv,  -- typings
>       inEnv  :: InEnv,  -- class instances
>       stEnv  :: StEnv   -- state types
>   }

>   initialEnvs :: Envs
>   initialEnvs = Envs M.empty M.empty M.empty M.empty M.empty M.empty

>   addADT :: String -> ADT -> Envs -> Envs
>   addADT n adt envs = envs { adtEnv = M.insert n adt (adtEnv envs) }

>   addAlias :: String -> Alias -> Envs -> Envs
>   addAlias n al envs = envs { alEnv = M.insert n al (alEnv envs) }

>   addClass :: String -> TypeClass -> Envs -> Envs
>   addClass n cl envs = envs { clEnv = M.insert n cl (clEnv envs) }

>   addExpl :: String -> PolyType -> Envs -> Either PolyType Envs
>   addExpl n ex envs = case M.lookup n (exEnv envs) of
>       Nothing   -> Right $ envs { exEnv = M.insert n ex (exEnv envs) }
>       (Just pt) -> Left pt

>   addAssumps :: Assumps -> Envs -> Envs
>   addAssumps as envs = envs { exEnv = as `M.union` (exEnv envs) }

>   addSt :: String -> StateType -> Envs -> Envs
>   addSt n st envs = envs { stEnv = M.insert n st (stEnv envs) }

>   getExpl :: String -> Envs -> Maybe PolyType
>   getExpl n envs = M.lookup n (exEnv envs)

>   getClsExpl :: String -> String -> Envs -> Maybe PolyType
>   getClsExpl cls n envs = do
>       cl <- M.lookup cls (clEnv envs)
>       M.lookup n (clAssumps cl)

>   addInstance :: String -> Instance -> Envs -> Envs 
>   addInstance n is envs = case M.lookup n (inEnv envs) of
>       Nothing    -> envs { inEnv = M.insert n [is] (inEnv envs) }
>       (Just iss) -> envs { inEnv = M.insert n (is : iss) (inEnv envs) }
    
    {----------------------------------------------------------------------}
    {-- Kinds                                                             -}
    {----------------------------------------------------------------------}

>   type KindIndex = M.Map String Kind

>   kinds :: HasKind a => Env a -> KindIndex
>   kinds = M.map kind

>   toKindIndex :: Envs -> KindIndex
>   toKindIndex (Envs adt als cls exs ins sts) = 
>       kinds adt `M.union` kinds als `M.union` kinds cls

>   toKindAssumps :: Envs -> Assumps
>   toKindAssumps = M.mapKeys ((:) 't') . M.map (mkPoly . demoteKind) . toKindIndex

>   kunion :: KindIndex -> KindIndex -> KindIndex
>   kunion = M.union

    {----------------------------------------------------------------------}
    {-- Assumps                                                           -}
    {----------------------------------------------------------------------}

>   mapUnions = M.foldl M.union M.empty
    
>   toAssumps :: Envs -> Assumps
>   toAssumps (Envs adt als cls exs ins sts) = 
>       mapUnions (M.map adtCtrs adt) `M.union` 
>       mapUnions (M.map clAssumps cls) `M.union`
>       exs
    
    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}

>   ppExpl :: (String, PolyType) -> ShowS
>   ppExpl (n,pt) = 
>       showString n .
>       ppTyping .
>       ppPolyType pt

>   ppInstances :: (String, [Instance]) -> ShowS
>   ppInstances (n,iss) = ppDefsW (ppInstance n) ppNewLine iss
    
>   ppEnvs :: Envs -> ShowS
>   ppEnvs (Envs adt als cls exs ins sts) =
>       showString "# Algebraic data types:\n" .
>       ppDefsW (uncurry ppADT) ppNewLine (M.toList adt) .
>       showString "\n# Type aliases:\n" .
>       ppDefsW (uncurry ppAlias) ppNewLine (M.toList als) .
>       showString "\n\n# Type classes:\n" .
>       ppDefsW (uncurry ppTypeClass) ppNewLine (M.toList cls) .
>       showString "\n\n# Explicit typings:\n" .
>       ppDefsW ppExpl ppNewLine (M.toList exs) .
>       showString "\n\n# Instances:\n" .
>       ppDefsW ppInstances ppNewLine (M.toList ins)
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
