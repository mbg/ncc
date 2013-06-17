{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.Substitution (
>   Theta(..),
>   epsilon,
>   (/),
>   Type(..),
>   (<>),
>   merge,
>   ppTheta
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Prelude hiding ((/))
>   import Debug.Trace (trace)
    
>   import qualified Data.Map as M
>   import qualified Data.Set as S

>   import TypeSystem.Types

>   import Utility.PrettyPrint

    {----------------------------------------------------------------------}
    {-- Substitutions                                                     -}
    {----------------------------------------------------------------------}
    
    A map of substitutions.
    
>   type Theta = M.Map TyVar MonoType 

    The empty substitution.

>   epsilon :: Theta
>   epsilon = M.empty

    Constructs a single substitution.

>   (/) :: TyVar -> MonoType -> Theta
>   x / t = M.singleton x t

    {----------------------------------------------------------------------}
    {-- Type Class                                                        -}
    {----------------------------------------------------------------------}
    
>   infixr 8 ~> 
>   class Type t where
>       (~>)   :: Theta -> t -> t
>       tyVars :: t -> S.Set TyVar
>       inst   :: [MonoType] -> t -> t
>
>       freeIn :: TyVar -> t -> Bool
>       freeIn tv t = tv `S.member` tyVars t
    
>   instance Type MonoType where
>       s ~> (TVar tv)   = case M.lookup tv s of
>           (Just t) -> t
>           Nothing  -> TVar tv
>       s ~> (TApp f a)  = TApp (s ~> f) (s ~> a)
>       s ~> (TFun f ps) = TFun f (s ~> ps)
>       s ~> t           = t
>
>       tyVars (TVar tv)   = S.singleton tv
>       tyVars (TApp f a)  = tyVars f `S.union` tyVars a
>       tyVars (TFun f ps) = tyVars ps
>       tyVars t           = S.empty
>
>       inst ts (TApp f a)   = TApp (inst ts f) (inst ts a)
>       inst ts (TGen n)
>           | n >= length ts = error "Substitution.inst"
>           | otherwise      = ts !! n
>       inst ts (TFun f ps)  = TFun f (inst ts ps)
>       inst ts t            = t
    
>   instance Type a => Type [a] where
>       s ~> ts = map (s ~>) ts
>       tyVars  = S.unions . map tyVars
>       inst ts = map (inst ts)

>   instance (Ord a, Type a) => Type (S.Set a) where
>       s ~> ts = S.map (s ~>) ts
>       tyVars  = S.foldl S.union S.empty . S.map tyVars 
>       inst ts = S.map (inst ts)

>   instance Type v => Type (M.Map k v) where
>       s ~> ts = M.map (s ~>) ts
>       tyVars  = tyVars . M.elems
>       inst ts = M.map (inst ts)

>   instance Type Constr where
>       s ~> (In n t)    = In n (s ~> t)
>       tyVars (In n t)  = tyVars t
>       inst ts (In n t) = In n (inst ts t)

>   instance Type t => Type (Qual t) where
>       s ~> (ctx :=> t)    = s ~> ctx :=> s ~> t
>       tyVars (_ :=> t)    = tyVars t 
>       inst ts (ctx :=> t) = inst ts ctx :=> inst ts t
    
    {----------------------------------------------------------------------}
    {-- Substitutions                                                     -}
    {----------------------------------------------------------------------}

    Like `Prelude.map', but for `Data.Map.Map' instead of lists.
    
>   mapAll :: (v -> Bool) -> M.Map k v -> Bool
>   mapAll p = M.foldr (\v b -> p v && b) True
    
    Composes two sets of substitutions. We apply the subsitutions in the
    first set to those of the second in the process.
    
>   infixr 4 <>
>   (<>) :: Theta -> Theta -> Theta
>   s1 <> s2 = M.map (s1 ~>) s2 `M.union` s1

    Tests if two sets of subsitutions agree on the substitutions they have
    in common. I.e. for each element in the intersection of both 
    substitutions, we test whether applying both substitutions results in
    the same mono type.

>   agree :: Theta -> Theta -> Bool
>   agree s1 s2 = mapAll p (s1 `M.intersection` s2)
>       where
>           p t = s1 ~> t == s2 ~> t

    Merges two sets of subsitutions if they agree (see above) or returns
    Nothing if not.

>   merge :: Theta -> Theta -> Maybe Theta
>   merge s1 s2
>       | agree s1 s2 = Just (s1 `M.union` s2)
>       | otherwise   = Nothing

    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}

>   ppTheta' :: TyVar -> MonoType -> ShowS -> ShowS
>   ppTheta' tv mt c =
>       ppTyVar tv .
>       showChar '/' .
>       ppMonoType 0 mt .
>       showChar ',' .
>       c
    
>   ppTheta :: Theta -> ShowS
>   ppTheta s = 
>       showChar '{' .
>       M.foldrWithKey ppTheta' id s .
>       showChar '}'
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
