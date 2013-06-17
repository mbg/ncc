{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

We use the same type inference algorithm for type as well as kind inference. In order to generate
useful error messages for each context, we use a data type for type errors instead of strings. 

> module TypeSystem.TypeError (
>   TypeError(..)
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Monad.Error.Class (Error(..))
    
>   import Cada.AST

>   import TypeSystem.Types 
>   import TypeSystem.PolyType

    {----------------------------------------------------------------------}
    {-- Type Errors                                                       -}
    {----------------------------------------------------------------------}

>   data TypeError = UnifyError MonoType MonoType
>                  | OccursCheck TyVar MonoType
>                  | KindCheck TyVar MonoType
>                  | MatchError MonoType MonoType
>                  | MergeError MonoType MonoType
>                  | NotInScope String
>                  | ConstrError Constr Constr
>                  | SignatureTooGeneral PolyType PolyType
>                  | ContextTooWeak Context
>                  | ContextReducationFailed Constr
>                  | OtherError String

    Recursive cases add context to an error:

>                  | UnifyAbs Expr Expr MonoType MonoType TypeError
>                  | ExprError Expr TypeError
>                  | OptionError Option TypeError
>                  | StmtError Statement TypeError
>                  | EqError Equation TypeError
>                  | EqsError String TypeError
>                  | OtherContext String TypeError
>                  | LocContext 

    For compatibility with ErrorT, we make TypeError an instance of Error.

>   instance Error TypeError where
>       strMsg = OtherError

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
