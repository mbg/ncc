{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Utility.State (
>   query,
>   queryOn
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Monad.State

    {----------------------------------------------------------------------}
    {-- Utility Functions                                                 -}
    {----------------------------------------------------------------------}

    Applies a function to the state like `modify', but additionally returns
    the old state.
    
>   query :: MonadState s m => (s -> s) -> m s
>   query f = get >>= \v -> put (f v) >> return v

    Gets a specific component of the state using the project function 
    supplied, modifies the state using another function which takes the
    component as argument and then returns the old value of the component.

>   queryOn :: MonadState s m => (s -> a) -> (a -> s -> s) -> m a
>   queryOn f g = gets f >>= \v -> modify (g v) >> return v

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
