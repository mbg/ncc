{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.Reduction (
>   entail,
>   reduce
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Debug.Trace
>   import TypeSystem.PrettyPrint
    
>   import Control.Applicative
>   import Control.Monad
>   import Control.Monad.Error
    
>   import Data.Foldable (foldrM)
>   import qualified Data.Map as M
>   import qualified Data.Set as S
    
>   import TypeSystem.Types
>   import TypeSystem.Substitution
>   import TypeSystem.TypeError
>   import TypeSystem.Monad
>   import TypeSystem.TypeClass
>   import TypeSystem.Instance
>   import TypeSystem.Environments
>   import TypeSystem.Unify

    {----------------------------------------------------------------------}
    {-- Entailment                                                        -}
    {----------------------------------------------------------------------}

>   bySuper :: ClEnv -> Constr -> TI Context
>   bySuper env p@(In n t) = case M.lookup n env of
>       (Just cls) -> do
>           ps <- mapM (bySuper env . flip In t) (map (\(In n _) -> n) $ S.toList $ clSupers cls)
>           return $ S.insert p (foldl S.union S.empty ps)
>       Nothing    -> throwError $ NotInScope n 
    
>   runAsMaybe :: TI a -> TI (Maybe a)
>   runAsMaybe m = (m >>= \x -> return (Just x)) `catchError` \_ -> return Nothing
    
>   tryInst :: Envs -> Constr -> Qual MonoType -> TI (Maybe Context)
>   tryInst env p@(In n _) (ps :=> h) = runAsMaybe $ do
>       s <- matchConstr env (In n h) p
>       return (s ~> ps)
    
>   byInst :: Envs -> Constr -> TI (Maybe Context)
>   byInst env p@(In n t) = case M.lookup n (inEnv env) of
>       (Just iss) -> do
>           rs <- mapM (tryInst env p . instType) iss
>           return $ msum rs
>       Nothing    -> return Nothing
    
>   entail :: Envs -> Context -> Constr -> TI Bool
>   entail env ctx p = do
>       bs <- any (S.member p) <$> mapM (bySuper (clEnv env)) (S.toList ctx)
>       bi <- byInst env p
>       be <- case bi of
>           Nothing -> return False
>           Just qs -> and <$> mapM (entail env ctx) (S.toList qs)
>       return $ bs || be 
    
    {----------------------------------------------------------------------}
    {-- Context Reduction                                                 -}
    {----------------------------------------------------------------------}
    
>   inHnf :: MonoType -> Bool
>   inHnf (TVar _)   = True
>   inHnf (TCtr _)   = False
>   inHnf (TFun _ _) = False
>   inHnf (TApp l _) = inHnf l

>   isHnf :: Constr -> Bool
>   isHnf (In n mt) = inHnf mt

>   toHnf :: Envs -> Constr -> TI Context
>   toHnf env c
>       | isHnf c   = return (S.singleton c)
>       | otherwise = do
>           mctx <- byInst env c 
>           case mctx of
>               Nothing    -> throwError $ ContextReducationFailed c
>               (Just ctx) -> toHnfs env ctx

>   toHnfs :: Envs -> Context -> TI Context
>   toHnfs env ctx = foldrM (\c cs -> S.union cs <$> toHnf env c) S.empty (S.toList ctx)
    
>   simplify :: Envs -> [Constr] -> [Constr] -> TI Context
>   simplify env rtx []     = return (S.fromList rtx)
>   simplify env rtx (c:cs) = do
>       b <- entail env (S.fromList (rtx++cs)) c
>       if b then 
>           simplify env rtx cs
>       else
>           simplify env (c:rtx) cs

>   reduce :: Envs -> Context -> TI Context 
>   reduce env ctx 
>       | S.null ctx = return ctx
>       | otherwise  = do
>           ctx' <- toHnfs env ctx
>           simplify env [] (S.toList ctx')