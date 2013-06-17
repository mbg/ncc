{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Internal.Kinds (
>   star,
>   arrow,
>   kindAssumps
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import qualified Data.Set as S
    
>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.PolyType
>   import TypeSystem.Assump

    {----------------------------------------------------------------------}
    {-- Kind Inference                                                    -}
    {----------------------------------------------------------------------}

>   star :: MonoType
>   star = TCtr $ TyCtr "Star" KStar

>   arrowType :: MonoType
>   arrowType = TCtr $ TyCtr "->" $ KFun KStar (KFun KStar KStar)

>   arrow :: MonoType -> MonoType -> MonoType
>   arrow f a = TApp (TApp arrowType f) a

>   tBool :: PolyType
>   tBool = mkPoly star

>   tInt :: PolyType
>   tInt = mkPoly star

>   tChar :: PolyType
>   tChar = mkPoly star 

>   tList :: PolyType
>   tList = mkPoly $ star `arrow` star

>   tArrow :: PolyType
>   tArrow = mkPoly $ star `arrow` (star `arrow` star)

>   tOption :: PolyType
>   tOption = ForAll [KStar, KStar] $ S.empty :=> (TGen 0) `arrow` ((TGen 1) `arrow` star)

>   tConstr :: PolyType
>   tConstr = mkPoly star
    
>   kindAssumps :: Assumps
>   kindAssumps = fromList [
>       ("tBool", tBool),
>       ("t[]", tList),
>       ("tInt", tInt),
>       ("tChar", tChar),
>       ("t->", tArrow),
>       ("+", tArrow),
>       ("|", tOption),
>       ("constraint", tConstr),
>       ("t(,)", tArrow),
>       ("t()", tInt),
>       ("tIO", tList)]

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
