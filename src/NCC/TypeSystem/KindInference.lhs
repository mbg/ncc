{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.KindInference (
>   inferKinds,
>   inferInstance,
>   inferPolyKind,
>   ppKindError
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Applicative
    
>   import qualified Data.Map as M
>   import Data.Maybe (fromJust)
    
>   import Cada.AST
>   import Cada.PrettyPrint
    
>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.PolyType
>   import TypeSystem.BindGroup
>   import TypeSystem.Assump
>   import TypeSystem.STypeDemotion
>   import TypeSystem.TypeInference 
>   import TypeSystem.TypeError
>   import TypeSystem.KindPromotion
>   import TypeSystem.Environments

>   import Internal.Kinds

>   import Utility.PrettyPrint
    
>   inferKinds :: Assumps -> BindGroup -> Either TypeError Assumps
>   inferKinds as = inferTypes initialEnvs (as <> kindAssumps)

>   extractKind :: Assumps -> Kind
>   extractKind = promotePolyType . fromJust . M.lookup "r"

>   inferInstance :: Assumps -> SInstance Loc -> Either TypeError Kind
>   inferInstance as inst = extractKind <$> inferTypes initialEnvs as' bg
>       where
>           e   = demoteInstance inst
>           bg  = BG [] [[Impl "r" [Eq "r" (Alt [] e)]]] []
>           as' = kindAssumps <> as

>   inferPolyKind :: Assumps -> TyScheme -> Either TypeError Kind
>   inferPolyKind as sc = extractKind <$> inferTypes initialEnvs as' bg
>       where
>           e   = demoteTypeS sc
>           bg  = BG [] [[Impl "r" [Eq "r" (Alt [] e)]]] []
>           as' = kindAssumps <> as
    
    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}
    
>   ppKindError :: TypeError -> ShowS
>   ppKindError (OtherError m)     = showString m
>   ppKindError (NotInScope n)     = showString "Not in scope: " . showString n
>   ppKindError (UnifyError t t')  = 
>       showString "Unify error:\n\tExpected: " . 
>       ppMonoType 0 t .
>       showString "\n\tActual: " .
>       ppMonoType 0 t'
>   ppKindError (OccursCheck tv t) = showString "Occurs check failed"
>   ppKindError (UnifyAbs fe ae t t' ex) =
>       ppKindError ex .
>       showString " while trying to unify the kinds of " .
>       ppId (ppExpr 0 fe) .
>       showString ":\n\t" .
>       ppMonoType 0 t' .
>       showString "\n and " .
>       ppId (ppExpr 0 ae) .
>       showString ":\n\t" .
>       ppMonoType 0 t .
>       ppNewLine
>   ppKindError (ExprError e ex)   = 
>       ppKindError ex .
>       showString "\n in type: " .
>       ppExpr 0 e
>   ppKindError (EqError e ex) =
>       ppKindError ex
>   ppKindError _ = showString "Unknown kind error"

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
