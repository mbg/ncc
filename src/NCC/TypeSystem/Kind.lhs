{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains Haskell types which represent kinds.

> module TypeSystem.Kind (
>   Kind(..),
>   HasKind(..),
>   kindArgs,
>   ppKind
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Utility.PrettyPrint
    
    {----------------------------------------------------------------------}
    {-- Kinds                                                             -}
    {----------------------------------------------------------------------}

    There are two kinds in Cada:
        *       : the kind of types
        k -> k  : kind functions

>   data Kind = KStar
>             | KFun Kind Kind 
>             deriving (Eq, Ord)

>   kindArgs :: Kind -> [Kind]
>   kindArgs KStar      = []
>   kindArgs (KFun a b) = a : kindArgs b

    {----------------------------------------------------------------------}
    {-- Type Class                                                        -}
    {----------------------------------------------------------------------}

>   class HasKind a where
>       kind :: a -> Kind
    
    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}

    Pretty prints a kind.
    
>   ppKind :: Bool -> Kind -> ShowS
>   ppKind _ KStar      = showChar '*'
>   ppKind b (KFun f a) = ppInOptParens b $
>       ppKind True f .
>       showString " -> " .
>       ppKind False a

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
