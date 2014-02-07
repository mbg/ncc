{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.Monad (
>   TI(..),
>   runTI,
>   reset,
>   getTheta,
>   unify,
>   unifyAbs,
>   newTyVar,
>   newInst,
>   inContext,
>   throwError
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Debug.Trace (trace)
    
>   import Control.Monad.Error
>   import Control.Monad.State

>   import Cada.AST (Expr(..))

>   import TypeSystem.Types
>   import TypeSystem.Substitution
>   import TypeSystem.Unify
>   import TypeSystem.PolyType
>   import TypeSystem.TypeError
>   import TypeSystem.Environments

>   import Utility.Counter
>   import Utility.PrettyPrint

    {----------------------------------------------------------------------}
    {-- Type Inference Monad                                              -}
    {----------------------------------------------------------------------}
    
>   type TE = StateT Theta Counter
>   type TI = ErrorT TypeError TE 

>   runTI :: TI a -> Either TypeError a
>   runTI = evalCounter . flip evalStateT epsilon . runErrorT

>   reset :: TI ()
>   reset = do
>       put epsilon
>       lift $ lift $ resetCounter

>   getTheta :: TI Theta
>   getTheta = lift get

>   freshTI :: TI Int
>   freshTI = lift (lift fresh)

>   extTheta :: Theta -> TI ()
>   extTheta s' = modify $ \s -> s' <> s

>   unify :: Envs -> MonoType -> MonoType -> TI ()
>   unify env t t' = do
>       s <- get
>       u <- mgu env (s ~> t) (s ~> t') --`inContext` (const $ UnifyError (s ~> t) (s ~> t'))
>       extTheta u

>   unifyAbs :: Envs -> Expr -> MonoType -> Expr -> MonoType -> TI ()
>   unifyAbs env ae t fe t' = do
>       s <- get
>       unify env t t' `inContext` \err -> 
>           UnifyAbs fe ae (s ~> t) (s ~> t') err

>   newTyVar :: Kind -> TI MonoType
>   newTyVar k = do 
>       i <- freshTI
>       return $ TVar $ TyVar ('t' : show i) k

>   newInst :: PolyType -> TI (Qual MonoType)
>   newInst (ForAll ks qt) = do
>       ts <- mapM newTyVar ks
>       return $ inst ts qt

>   inContext :: TI a -> (TypeError -> TypeError) -> TI a
>   inContext m f = m `catchError` (throwError . f) 
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
