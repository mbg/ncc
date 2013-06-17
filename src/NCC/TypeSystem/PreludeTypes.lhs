{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.PreludeTypes (
>   listType,
>   unitType,
>   intType,
>   boolType,
>   stringType,
>   preludeKindIndex
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Data.Map (fromList)
    
>   import TypeSystem.Types
>   import TypeSystem.Environments
    
    {----------------------------------------------------------------------}
    {-- Built-in Types                                                    -}
    {----------------------------------------------------------------------}
       
>   listType :: MonoType
>   listType = TCtr $ TyCtr "[]" $ KFun KStar KStar

>   mkListType :: MonoType -> MonoType
>   mkListType t = TApp listType t
    
>   unitType :: MonoType
>   unitType = TCtr $ TyCtr "()" KStar
    
>   intType :: MonoType
>   intType = TCtr $ TyCtr "Int" KStar

>   boolType :: MonoType
>   boolType = TCtr $ TyCtr "Bool" KStar

>   stringType :: MonoType
>   stringType = TFun (TyFun "String" KStar) []

    {----------------------------------------------------------------------}
    {-- Kind Index                                                        -}
    {----------------------------------------------------------------------}
   
>   preludeKindIndex :: KindIndex
>   preludeKindIndex = fromList $ [
>       ("()", KStar),
>       ("Int", KStar),
>       ("->", KFun KStar (KFun KStar KStar)),
>       ("[]", KFun KStar KStar),
>       ("(,)", KFun KStar (KFun KStar KStar)),
>       ("IO", KFun KStar KStar)]
   
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
