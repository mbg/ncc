{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains the data types for the internal representation of type aliases.

> module TypeSystem.Alias (
>   Alias(..),
>   ppAlias
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import TypeSystem.Kind
>   import TypeSystem.PolyType

>   import Utility.PrettyPrint (ppInParens)

    {----------------------------------------------------------------------}
    {-- Type Aliases                                                      -}
    {----------------------------------------------------------------------}

>   data Alias = Alias {
>       aliasKind  :: Kind,
>       aliasArity :: Int,
>       aliasType  :: PolyType
>   }

>   instance HasKind Alias where
>       kind = aliasKind

    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}

>   ppAlias :: String -> Alias -> ShowS
>   ppAlias n (Alias k a pt) =
>       showString "type " .
>       showString n .
>       ppInParens (showString "arity " . shows a) .
>       showString " : " .
>       ppKind False k .
>       showString " = " .
>       ppPolyType pt
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
