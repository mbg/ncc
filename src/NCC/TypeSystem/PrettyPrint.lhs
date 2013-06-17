{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.PrettyPrint (
>   ppTypeError
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import qualified Data.Set as S
    
>   import Utility.PrettyPrint

>   import Cada.AST
>   import Cada.PrettyPrint

>   import TypeSystem.Types
>   import TypeSystem.PolyType
>   import TypeSystem.TypeError

    {----------------------------------------------------------------------}
    {-- Type Errors                                                       -}
    {----------------------------------------------------------------------}
        
>   inExprError :: Expr -> ShowS
>   inExprError e =
>       ppTab . 
>       showString " in the expression:\n" .
>       ppIndent 2 .
>       ppExpr 2 e .
>       ppNewLine 

>   inStmtError :: Statement -> ShowS
>   inStmtError s =
>       ppTab .
>       showString " in the statement:\n" .
>       ppStatement 2 s .
>       ppNewLine

>   inCaseOptError :: Option -> ShowS
>   inCaseOptError opt =
>       ppTab .
>       showString " in a case alternative:\n" .
>       ppOption 2 opt .
>       ppNewLine
    
>   inEqError :: Equation -> ShowS
>   inEqError eq@(Eq n _) =
>       ppTab .
>       showString " in an equation for " .
>       ppId (showString n) .
>       showString ":\n" .
>       ppEquation' 2 eq .
>       ppNewLine

>   ppTypeError :: TypeError -> ShowS
>   ppTypeError (UnifyError t t') =
>       ppTab .
>       showString "Couldn't unify types:\n" .
>       ppIndent 2 .
>       showString "Actual: " .
>       ppMonoType 0 t .
>       ppNewLine .
>       ppIndent 2 .
>       showString "Expected: " .
>       ppMonoType 0 t' .
>       ppNewLine
>   ppTypeError (MatchError t t') =
>       ppTab .
>       showString "Couldn't match:\n" .
>       ppIndent 2 .
>       ppMonoType 0 t .
>       ppNewLine .
>       ppIndent 2 .
>       ppMonoType 0 t'
>   ppTypeError (KindCheck tv t) =
>       ppTab .
>       showString "Kind check: \n" .
>       ppIndent 2 .
>       ppId (ppTyVar tv) .
>       showString " has kind: " .
>       ppKind False (kind tv) .
>       ppNewLine .
>       ppIndent 2 .
>       showString "but " .
>       ppId (ppMonoType 0 t) .
>       showString " has kind: " .
>       ppKind False (kind t) .
>       ppNewLine
>   ppTypeError (OccursCheck tv t) =
>       ppTab .
>       showString "Occurs check: cannot construct the infinite type\n" .
>       ppIndent 2 .
>       ppTyVar tv .
>       showString " := " .
>       ppMonoType 0 t .
>       ppNewLine
>   ppTypeError (NotInScope x) =
>       ppTab .
>       showString "Not in scope: " .
>       ppId (showString x) .
>       ppNewLine
>   ppTypeError (SignatureTooGeneral et at) =
>       ppTab .
>       showString "Signature too general:\n" .
>       ppIndent 2 .
>       showString "Expected type: " .
>       ppPolyType et .
>       ppNewLine .
>       ppIndent 2 .
>       showString "Inferred type: " .
>       ppPolyType at .
>       ppNewLine
>   ppTypeError (ContextTooWeak ctx) =
>       ppTab .
>       showString "The context of an explicitly typed binding is too weak, because\n" .
>       ppTab .
>       showString "it can't satisfy the following constraint(s):\n" .
>       ppIndent 2 .
>       ppDefsW ppConstr (ppNewLine . ppIndent 2) (S.toList ctx) .
>       ppNewLine
>   ppTypeError (ContextReducationFailed c) =
>       ppTab .
>       showString "Context reduction failed, no instance for:\n" .
>       ppIndent 2 .
>       ppConstr c .
>       ppNewLine
>   ppTypeError (UnifyAbs fe ae t t' err) =
>       ppTypeError err .
>       ppTab . 
>       showString " while trying to unify the types of " .
>       ppId (ppExpr 0 fe) .
>       showString ":\n" .
>       ppIndent 2 .
>       ppMonoType 0 t' .
>       ppNewLine .
>       ppTab .
>       showString " and " .
>       ppId (ppExpr 0 ae) .
>       showString ":\n" .
>       ppIndent 2 .
>       ppMonoType 0 t .
>       ppNewLine
>   ppTypeError (ExprError e err) =
>       ppTypeError err .
>       inExprError e 
>   ppTypeError (StmtError s err) =
>       ppTypeError err .
>       inStmtError s
>   ppTypeError (OptionError o err) =
>       ppTypeError err .
>       inCaseOptError o
>   ppTypeError (EqError e err) =
>       ppTypeError err .
>       inEqError e
>   ppTypeError (EqsError n err) =
>       ppTypeError err .
>       ppTab .
>       showString " in the equations for " .
>       ppId (showString n)
>   ppTypeError (OtherError msg) =
>       ppTab .
>       showString "Unable to infer the types of the program:\n" .
>       ppIndent 2 .
>       showString msg 
>   ppTypeError _ = showString "Type error"

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
