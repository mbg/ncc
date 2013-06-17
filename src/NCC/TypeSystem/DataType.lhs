{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains the data types for the internal representation of data types.

> module TypeSystem.DataType (
>   ADT(..),
>   ppADT
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import TypeSystem.Kind
>   import TypeSystem.Assump

    {----------------------------------------------------------------------}
    {-- Algebraic Data Types                                              -}
    {----------------------------------------------------------------------}

>   data ADT = ADT {
>       adtKind :: Kind,
>       adtCtrs :: Assumps
>   }

>   instance HasKind ADT where
>       kind = adtKind

    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}

>   ppADT :: String -> ADT -> ShowS
>   ppADT n (ADT k as) =
>       showString "data " .
>       showString n .
>       showString " : " .
>       ppKind False k .
>       showString " where\n" .
>       ppAssumps 1 as
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
