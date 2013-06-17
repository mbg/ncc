{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.PolyType (
>   PolyType(..),
>   quantifyTy,
>   mkPoly,
>   mkMono,
>   ppPolyType
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Applicative
>   import Control.Monad.State
    
>   import qualified Data.Set as S
>   import qualified Data.Map as M
    
>   import TypeSystem.Types
>   import TypeSystem.Substitution

>   import Utility.PrettyPrint

    {----------------------------------------------------------------------}
    {-- Polymorphic Types                                                 -}
    {----------------------------------------------------------------------}

    Represents polymorphic types.

>   data PolyType = ForAll [Kind] (Qual MonoType)

>   instance HasContext PolyType where
>       context (ForAll _ qt) = context qt

>   instance Type PolyType where
>       s ~> (ForAll ks qt)  = pt
>           where
>               qt' = s ~> qt
>               vs  = tyVars qt'
>               pt  = quantifyTy vs qt'
>       tyVars (ForAll _ qt) = tyVars qt
>       inst ts (ForAll ks qt) = pt
>           where
>               qt' = inst ts qt
>               vs  = tyVars qt'
>               pt  = quantifyTy vs qt' 

>   quantifyTy :: S.Set TyVar -> Qual MonoType -> PolyType
>   quantifyTy vs qt = ForAll ks (s ~> qt)
>       where
>           vs' = S.toList (tyVars qt)
>           ks  = map kind vs'
>           s   = M.fromList $ zip vs' (map TGen [0..])

    Turns a monomorphic type into a polymorphic type (using magic).

>   mkPoly :: MonoType -> PolyType
>   mkPoly t = ForAll [] (S.empty :=> t)

>   mkMono :: PolyType -> MonoType
>   mkMono (ForAll _ (_ :=> t)) = t

    {----------------------------------------------------------------------}
    {-- Polymorphic type equality                                         -}
    {----------------------------------------------------------------------}

>   type VarMap = M.Map Int Int
>   type TyComp = State VarMap

>   monoTypeEq :: MonoType -> MonoType -> TyComp Bool
>   monoTypeEq (TGen n) (TGen n') = do
>       ms <- get
>       case M.lookup n ms of
>           (Just m) -> return (n' == m)
>           Nothing  -> do
>               put (M.insert n n' ms)
>               return True
>   monoTypeEq (TApp f a) (TApp f' a') = do
>       rf <- monoTypeEq f f'
>       ra <- monoTypeEq a a'
>       return (rf && ra)
>   monoTypeEq (TFun f ts) (TFun f' ts') 
>       | length ts == length ts' = do
>           rs <- and <$> mapM (uncurry monoTypeEq) (zip ts ts')
>           return (f == f' && rs)
>       | otherwise = return False
>   monoTypeEq mt mt' = return (mt == mt')

>   typeEq :: Qual MonoType -> Qual MonoType -> TyComp Bool
>   typeEq (ctx :=> mt) (ctx' :=> mt') = do
>       mr <- monoTypeEq mt mt'
>       return mr

>   instance Eq PolyType where
>       (ForAll _ qt) == (ForAll _ qt') = 
>           evalState (typeEq qt qt') M.empty

    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}

>   ppPolyType :: PolyType -> ShowS
>   ppPolyType (ForAll [] qt) = ppQual qt
>   ppPolyType (ForAll ks qt) = 
>       showString "forall " .
>       ppDefsW (ppKind False) (showChar ',') ks .
>       showChar '.' .
>       ppQual qt  

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
