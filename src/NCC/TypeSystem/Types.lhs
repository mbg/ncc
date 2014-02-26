{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains the data types for Cada's type system. Note that these types are internal
representations as opposed to "surface" representations which are used in the AST.

> module TypeSystem.Types (
>   module TypeSystem.Kind,
>   TyVar(..),
>   TyCtr(..),
>   TyFun(..),
>   MonoType(..),
>   Constr(..),
>   Context,
>   Qual(..),
>   HasContext(..),
>   funType,
>   mkFun,
>   isArrow,
>   ppTyVar,
>   ppTyCtr,
>   ppMonoType,
>   ppConstr,
>   ppContext,
>   ppQual
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import qualified Data.Set as S
    
>   import TypeSystem.Kind

>   import Utility.PrettyPrint

    {----------------------------------------------------------------------}
    {-- Types                                                             -}
    {----------------------------------------------------------------------}

    Type variables and type constructors have names as well as kinds. For
    convenience, we define data types for them s.t. we don't have to use
    pairs if we need both.
    
>   data TyVar = TyVar String Kind deriving Eq
>   data TyCtr = TyCtr String Kind deriving (Eq, Ord)
>   data TyFun = TyFun String Kind deriving (Eq, Ord)
    
>   instance Ord TyVar where
>       compare (TyVar n1 _) (TyVar n2 _) = compare n1 n2 
    
    All three have kinds.
    
>   instance HasKind TyVar where
>       kind (TyVar _ k) = k

>   instance HasKind TyCtr where
>       kind (TyCtr _ k) = k

>   instance HasKind TyFun where
>       kind (TyFun _ k) = k
    
    Mono types.
    
>   data MonoType = TVar TyVar
>                 | TCtr TyCtr
>                 | TFun TyFun [MonoType]
>                 | TApp MonoType MonoType
>                 -- special types which are only used during type inference
>                 | TGen Int
>                 deriving (Eq, Ord)

>   codomain :: Int -> Kind -> Kind
>   codomain 0 k          = k
>   codomain n (KFun a b) = codomain (n-1) b

>   instance HasKind MonoType where
>       kind (TVar tv)    = kind tv
>       kind (TCtr tc)    = kind tc
>       kind (TFun tf ps) = codomain (length ps) (kind tf)
>       kind (TApp f a)   = let (KFun _ k) = kind f in k
    
    A constraint 
    
>   data Constr  = In String MonoType
>                  deriving (Eq, Ord)

>   type Context = S.Set Constr
    
    We can add contexts to types.
    
>   infixr 5 :=>
>   data Qual t = Context :=> t deriving Eq

>   class HasContext t where
>       context :: t -> Context

>   instance HasContext (Qual t) where
>       context (ctx :=> _) = ctx

>   instance HasKind t => HasKind (Qual t) where
>       kind (_ :=> t) = kind t

>   funType :: MonoType
>   funType = TCtr $ TyCtr "->" $ KFun KStar (KFun KStar KStar)
    
>   mkFun :: MonoType -> MonoType -> MonoType
>   mkFun ft at = TApp (TApp funType ft) at
    
    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}
    
>   isList :: MonoType -> Bool
>   isList (TCtr (TyCtr "[]" _)) = True
>   isList _                     = False
    
>   isArrow :: MonoType -> Bool
>   isArrow (TCtr (TyCtr "->" _)) = True
>   isArrow _                     = False

>   isTuple :: MonoType -> Bool
>   isTuple (TCtr (TyCtr "(,)" _)) = True
>   isTuple _                      = False
    
>   ppTyVar :: TyVar -> ShowS
>   ppTyVar (TyVar n k) = -- ppInParens $
>       showString n -- . ppTyping . ppKind False k

>   ppTyCtr :: TyCtr -> ShowS
>   ppTyCtr (TyCtr n k) = -- ppInParens $
>       showString n -- . ppTyping . ppKind False k

>   ppTyFun :: TyFun -> ShowS
>   ppTyFun (TyFun n k) = --showString "{-%-}" . -- ppInParens $
>       showString n -- . ppTyping . ppKind False k

>   ppMonoType :: Int -> MonoType -> ShowS
>   ppMonoType _ (TVar x)    = ppTyVar x
>   ppMonoType _ (TCtr c)    = ppTyCtr c
>   ppMonoType _ (TFun f []) = ppTyFun f
>   ppMonoType p (TFun f ps) = ppInOptParens (p>=10) $ 
>       ppTyFun f . ppSpace . ppDefsW (ppMonoType 5) ppSpace ps
>   ppMonoType _ (TGen n)    = showChar 't' . showString (show n)
>   ppMonoType p (TApp (TApp f a) b)
>       | isArrow f = ppInOptParens (p>=5) $ ppMonoType 5 a . showString " -> " . ppMonoType 0 b
>       | isTuple f = ppInParens $ ppMonoType 0 a . showChar ',' . ppMonoType 0 b
>   ppMonoType p (TApp f a)
>       | isList f  = ppList $ ppMonoType 0 a
>       | otherwise = ppInParens {-ppInOptParens (p>=10)-} $
>           ppMonoType 5 f .
>           ppSpace .
>           ppMonoType 10 a

>   ppConstr :: Constr -> ShowS
>   ppConstr (In n t) = showString n . ppSpace . ppMonoType 10 t

>   ppContext' :: [Constr] -> ShowS
>   ppContext' [c] = ppConstr c
>   ppContext' ctx = ppInParens $ 
>       ppDefsW ppConstr (showChar ',') ctx 

>   ppContext :: Context -> ShowS
>   ppContext ctx
>       | S.null ctx = id
>       | otherwise  = 
>           ppContext' (S.toList ctx) .
>           showString " => "

>   ppQual :: Qual MonoType -> ShowS
>   ppQual (ctx :=> t) 
>       | S.null ctx = ppMonoType 0 t
>       | otherwise  = 
>           ppContext ctx .
>           ppMonoType 0 t
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
