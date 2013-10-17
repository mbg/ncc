{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Internal.NCC (
>   pairType,
>   listPolyType,
>   mkListType,
>   nccInternal
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import qualified Data.Map as M
>   import qualified Data.Set as S
    
>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.PolyType
>   import TypeSystem.DataType
>   import TypeSystem.Assump
>   import TypeSystem.Environments

    {----------------------------------------------------------------------}
    {-- NCC.Internal                                                      -}
    {----------------------------------------------------------------------}
    
>   unitType :: MonoType
>   unitType = TCtr $ TyCtr "()" KStar

>   intType :: MonoType
>   intType = TCtr $ TyCtr "Int" KStar

>   charType :: MonoType
>   charType = TCtr $ TyCtr "Char" KStar

>   boolType :: MonoType
>   boolType = TCtr $ TyCtr "Bool" KStar
    
>   pairType :: MonoType
>   pairType = TCtr $ TyCtr "(,)" $ KFun KStar (KFun KStar KStar)

>   mkPair :: MonoType -> MonoType -> MonoType
>   mkPair a b = TApp (TApp pairType a) b

>   listType :: MonoType
>   listType = TCtr $ TyCtr "[]" $ KFun KStar KStar

>   listPolyType :: PolyType
>   listPolyType = ForAll [KStar] (S.empty :=> mkListType (TGen 0))

>   mkListType :: MonoType -> MonoType
>   mkListType t = TApp listType t

>   stringTy :: MonoType
>   stringTy = TFun (TyFun "String" KStar) []

>   ioTy :: MonoType
>   ioTy = TCtr $ TyCtr "IO" (KFun KStar KStar)

>   nccADTs :: ADTEnv
>   nccADTs = M.fromList [
>       ("IO", ADT (KFun KStar KStar) M.empty)]

>   nccInternalAliases :: AlEnv
>   nccInternalAliases = M.fromList []
    
>   nccInternalExpls :: ExEnv
>   nccInternalExpls = M.fromList [
>       ("True", mkPoly boolType),
>       ("False", mkPoly boolType),
>       ("(,)", ForAll [KStar,KStar] (S.empty :=> TGen 0 `mkFun` (TGen 1 `mkFun` mkPair (TGen 0) (TGen 1)))),
>       ("[]", ForAll [KStar] (S.empty :=> mkListType (TGen 0))),
>       (":", ForAll [KStar] (S.empty :=> TGen 0 `mkFun` (mkListType (TGen 0) `mkFun` mkListType (TGen 0)))),
>       ("error", ForAll [KStar] (S.empty :=> TGen 0)),

>       ("intEq", ForAll [] (S.empty :=> intType `mkFun` (intType `mkFun` boolType))),
>       ("intIneq", ForAll [] (S.empty :=> intType `mkFun` (intType `mkFun` boolType))),
>       ("intAdd", mkPoly (intType `mkFun` (intType `mkFun` intType))),
>       ("intSub", mkPoly (intType `mkFun` (intType `mkFun` intType))),
>       ("intMul", mkPoly (intType `mkFun` (intType `mkFun` intType))),
>       ("intNegate", mkPoly (intType `mkFun` intType)),
>       ("intAbs", mkPoly (intType `mkFun` intType)),
>       ("div", mkPoly (intType `mkFun` (intType `mkFun` intType))),
>       ("intShow", mkPoly (intType `mkFun` stringTy)),

>       ("charEq", mkPoly (charType `mkFun` (charType `mkFun` boolType))),
>       ("charIneq", mkPoly (charType `mkFun` (charType `mkFun` boolType))),
>       ("charShow", mkPoly (charType `mkFun` stringTy)),

>       ("returnIO", ForAll [KStar] (S.empty :=> TGen 0 `mkFun` (TApp ioTy (TGen 0)))),
>       ("bindIO", ForAll [KStar,KStar] (S.empty :=> (TApp ioTy (TGen 0)) `mkFun` ((TGen 0 `mkFun` (TApp ioTy (TGen 1))) `mkFun` (TApp ioTy (TGen 1))))),

>       ("putStrLn", mkPoly (stringTy `mkFun` (TApp ioTy unitType))),
>       ("getLine", mkPoly (TApp ioTy stringTy))]

>   nccInternal :: Envs
>   nccInternal = Envs nccADTs nccInternalAliases M.empty nccInternalExpls M.empty M.empty

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
