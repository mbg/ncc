{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.KindPromotion (
>   promotePolyType,
>   promoteMonoType
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.Substitution
>   import TypeSystem.PolyType 
>   import TypeSystem.PreludeKinds

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   promotePolyType :: PolyType -> Kind
>   promotePolyType = promoteMonoType . instKind
    
>   promoteMonoType :: MonoType -> Kind
>   promoteMonoType (TCtr (TyCtr "Star" _))                  = KStar 
>   promoteMonoType (TApp (TApp (TCtr (TyCtr "->" _)) t) t') = 
>       KFun (promoteMonoType t) (promoteMonoType t')

>   instKind :: PolyType -> MonoType
>   instKind (ForAll ks (_ :=> t)) = inst (map (const star) ks) t

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
