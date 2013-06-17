{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains the data types for the internal representation of type classes.

> module TypeSystem.TypeClass (
>   TypeClass(..),
>   ppTypeClass
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.Assump

    {----------------------------------------------------------------------}
    {-- Type Aliases                                                      -}
    {----------------------------------------------------------------------}

>   data TypeClass = TypeClass {
>       clSupers  :: Context,
>       clKind    :: Kind,
>       clAssumps :: Assumps
>   }

>   instance HasKind TypeClass where
>       kind = clKind

    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}
    
>   ppTypeClass :: String -> TypeClass -> ShowS
>   ppTypeClass n (TypeClass ctx k as) =
>       showString "class " .
>       ppContext ctx .
>       showString n .
>       showString " : " .
>       ppKind False k .
>       showString " where\n" .
>       ppAssumps 1 as
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
