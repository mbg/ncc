{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

Small implementation of difference lists - similar to Data.DList

> module Utility.Accum (
>   Accum(..),
>   unA,
>   consA,
>   contA,
>   mapA
> ) where

    {----------------------------------------------------------------------}
    {-- Accumulation                                                      -}
    {----------------------------------------------------------------------}

    Left-recursive LALR parsers are more efficient, but cause problems with
    the list type since we can only add elements at the beginning of a list,
    not the end. To circumvent this issue, we use accumulators. The idea is
    that each parser constructs a continuation which we can then apply to
    lists.
    
>   type Accum a = [a] -> [a]

    Generates a list from a continuation by applying it to the empty list.

>   unA :: Accum a -> [a]
>   unA f = f []

    Adds an element to the accumulator and returns the resulting list. Together
    with the identity function, this is one possible base case.

>   consA :: a -> Accum a
>   consA = (:)

    Adds an element to the accumulator and applies the continuation to the
    resulting list. This is used for recursive definitions.

>   contA :: Accum a -> a -> Accum a
>   contA f x xs = f (x : xs)

    Maps an accumulating function to all elements of a list and returns the
    resulting function.

>   mapA :: (a -> Accum b) -> [a] -> Accum b
>   mapA f []     = id
>   mapA f (x:xs) = f x . mapA f xs 

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
