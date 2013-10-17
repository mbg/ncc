{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Compiler.Binary (
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Debug.Trace (trace)
>   import Utility.PrettyPrint
    
>   import Control.Applicative
>   import Control.Monad
>   import Control.Monad.IO.Class
    
>   import Data.Binary
>   import Data.Binary.Get
>   import Data.Binary.Put
>   import qualified Data.Map as M
>   import qualified Data.Set as S

>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.PolyType
>   import TypeSystem.DataType
>   import TypeSystem.Alias
>   import TypeSystem.TypeClass
>   import TypeSystem.Instance
>   import TypeSystem.StateType
>   import TypeSystem.Environments

    {----------------------------------------------------------------------}
    {-- Helper Instances                                                  -}
    {----------------------------------------------------------------------}

    
    {----------------------------------------------------------------------}
    {-- De/Serialisation                                                  -}
    {----------------------------------------------------------------------}
    
>   putList :: Binary a => [a] -> Put
>   putList xs = do
>       put (length xs)
>       mapM_ put xs

>   getList :: Binary a => Get [a]
>   getList = do
>       c <- get
>       replicateM c get
    
>   instance Binary Kind where
>       put KStar      = putWord8 0
>       put (KFun a b) = do
>           putWord8 1
>           put a
>           put b
>
>       get = do
>           c <- getWord8
>           case c of
>               0 -> return KStar
>               1 -> KFun <$> get <*> get

>   instance Binary TyVar where
>       put (TyVar n k) = do
>           put n
>           put k
>
>       get = TyVar <$> get <*> get

>   instance Binary TyCtr where
>       put (TyCtr n k) = do
>           put n
>           put k
>
>       get = TyCtr <$> get <*> get

>   instance Binary TyFun where
>       put (TyFun n k) = do
>           put n
>           put k
>
>       get = TyFun <$> get <*> get

>   instance Binary MonoType where
>       put (TVar v)    = putWord8 0 >> put v
>       put (TCtr c)    = putWord8 1 >> put c
>       put (TGen n)    = putWord8 2 >> put n
>       put (TApp a b)  = do
>           putWord8 3
>           put a
>           put b
>       put (TFun f ps) = do
>           putWord8 4
>           put f
>           putList ps
>
>       get = do
>           c <- getWord8
>           case c of
>               0 -> TVar <$> get
>               1 -> TCtr <$> get
>               2 -> TGen <$> get 
>               3 -> TApp <$> get <*> get 
>               4 -> do
>                   f  <- get
>                   ps <- getList 
>                   return $ TFun f ps

>   instance Binary Constr where
>       put (In n mt) = do
>           put n
>           put mt
>
>       get = In <$> get <*> get

>   instance (Binary t) => Binary (Qual t) where
>       put (ctx :=> mt) = do
>           put ctx 
>           put mt
>
>       get = (:=>) <$> get <*> get

>   instance Binary PolyType where
>       put (ForAll ks qt) = do
>           put (length ks)
>           mapM_ put ks
>           put qt
>
>       get = do
>           c  <- get
>           ks <- replicateM c get
>           qt <- get
>           return $ ForAll ks qt

>   instance Binary ADT where
>       put (ADT k as) = do
>           put k
>           put (M.size as)
>           mapM_ put (M.toList as)
>
>       get = do
>           k  <- get
>           c  <- get
>           as <- M.fromList <$> replicateM c get
>           return $ ADT k as

>   instance Binary Alias where
>       put (Alias k a pt) = do
>           put k
>           put a
>           put pt
>
>       get = Alias <$> get <*> get <*> get

>   instance Binary TypeClass where
>       put (TypeClass ctx k as) = do
>           put ctx
>           put k
>           put (M.size as)
>           mapM_ put (M.toList as)
>
>       get = do
>           ctx <- get
>           k   <- get
>           c   <- get
>           as  <- M.fromList <$> replicateM c get
>           return $ TypeClass ctx k as

>   instance Binary Instance where
>       put (Inst qt _) = do
>           put qt
>
>       get = Inst <$> get <*> pure []

>   instance Binary StateType where
>       put Simple      = putWord8 0
>       put Transformer = putWord8 1
>
>       get = do
>           c <- getWord8
>           case c of
>               0 -> pure Simple
>               1 -> pure Transformer

>   instance Binary Envs where
>       put = undefined
>       get = undefined

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
