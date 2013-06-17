{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains the ``surface'' representation of types. In other words, the data types which
are defined in this module are part of the AST and will later be converted to real types.

> module Cada.STypes (
>   FreeTyVars(..),
>   TypeName,
>   TypeConstraint(..),
>   TypeParam(..),
>   SType(..),
>   sTypeDomain,
>   ctrsFromType,
>   occursIn,
>   TyQual(..),
>   TyScheme(..),
>   quantify,
>   tyUnit,
>   tyArrow
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import qualified Data.Set as S
    
>   import Cada.Lexer
>   import Cada.Location

>   import Utility.Accum

    {----------------------------------------------------------------------}
    {-- Type Classes                                                      -}
    {----------------------------------------------------------------------}
    
>   class FreeTyVars a where
>       freeTyVars  :: a -> S.Set SType
>       freeTyVarsN :: a -> S.Set String
>       freeTyVarsN = S.map tyVarName . freeTyVars

>   instance FreeTyVars a => FreeTyVars [a] where
>       freeTyVars = foldr S.union S.empty . map freeTyVars

    {----------------------------------------------------------------------}
    {-- Types                                                             -}
    {----------------------------------------------------------------------}

>   type TypeName   = String
    
>   data TypeConstraint = TyConstr {
>       tyConstrClass :: TypeName,
>       tyConstrVar   :: String
>   }

>   instance FreeTyVars TypeConstraint where
>       freeTyVars (TyConstr _ v) = S.singleton (STyVar v)

>   data TypeParam = TyP {
>       tyParamName  :: String,
>       tyParamPos   :: AlexPosn
>   }

>   instance Positioned TypeParam where
>       srcPos = FilePos . tyParamPos

>   instance Eq TypeParam where
>       (TyP x _) == (TyP y _) = x == y

>   data SType = STyVar {
>       tyVarName :: String
>   }          | STyCtr {
>       tyCtrName :: String
>   }          | STyApp {
>       tyAppFun  :: SType,
>       tyAppArg  :: SType
>   }          | STyTuple {
>       tyTuple   :: [SType]
>   }          | STyList {
>       tyList    :: SType
>   } deriving (Eq, Ord)

>   sTypeDomain :: SType -> SType
>   sTypeDomain (STyApp l _) = sTypeDomain l
>   sTypeDomain t            = t

>   ctrsFromType :: SType -> Accum String
>   ctrsFromType (STyCtr n)    = consA n
>   ctrsFromType (STyApp l r)  = ctrsFromType l . ctrsFromType r
>   ctrsFromType (STyTuple ps) = mapA ctrsFromType ps
>   ctrsFromType (STyList p)   = ctrsFromType p
>   ctrsFromType _             = id

>   instance FreeTyVars SType where
>       freeTyVars v@(STyVar _)  = S.singleton v
>       freeTyVars (STyCtr _)    = S.empty
>       freeTyVars (STyApp l r)  = freeTyVars l `S.union` freeTyVars r
>       freeTyVars (STyTuple ts) = freeTyVars ts
>       freeTyVars (STyList t)   = freeTyVars t

>   occursIn :: String -> SType -> Bool
>   occursIn n (STyVar v)    = n == v
>   occursIn n (STyCtr c)    = n == c
>   occursIn n (STyApp l r)  = occursIn n l || occursIn n r
>   occursIn n (STyTuple ts) = any (occursIn n) ts
>   occursIn n (STyList t)   = occursIn n t

>   data TyQual t = [TypeConstraint] :==> t
>   data TyScheme = Scheme [String] (TyQual SType)

>   instance FreeTyVars t => FreeTyVars (TyQual t) where
>       freeTyVars (ctx :==> t) = freeTyVars ctx `S.union` freeTyVars t

>   quantify :: TyQual SType -> TyScheme
>   quantify qt = Scheme (S.toList $ freeTyVarsN qt) qt

>   tyUnit :: SType
>   tyUnit = STyCtr "()" 

>   tyArrow :: SType -> SType -> SType
>   tyArrow x y = STyApp (STyApp (STyCtr "->") x) y

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
