{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Utility.Errors (
>   module Utility.StateM,
>
>   Errors,
>   toEither,
>   addError,
>   addFatalError
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Utility.Accum
>   import Utility.StateM

    {----------------------------------------------------------------------}
    {-- Errors Monad                                                      -}
    {----------------------------------------------------------------------}
    
>   type Errors e = StateM (Accum e) 

>   toEither :: Errors e a -> Either [e] a
>   toEither m = case runStateM m id of
>       (Nothing, err) -> Left (unA err)
>       (Just x, _)    -> Right x

>   addError :: e -> Errors e ()
>   addError = modify . flip contA

>   addFatalError :: e -> Errors e a
>   addFatalError ex = addError ex >> failMT

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
