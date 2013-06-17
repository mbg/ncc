{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Utility.Monad (
>   maybeDo,
>   queryOn,
>   query
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import Control.Monad.State.Class

    {----------------------------------------------------------------------}
    {-- Helper Functions                                                  -}
    {----------------------------------------------------------------------}

>   maybeDo :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
>   maybeDo _ Nothing  = return ()
>   maybeDo m (Just x) = m x

    Gets a specific component of the state using the given project function, 
    modifies the state using another function which takes the
    component as argument and then returns the old value of the component.

>   queryOn :: MonadState s m => (s -> a) -> (a -> s -> s) -> m a
>   queryOn f g = gets f >>= \v -> modify (g v) >> return v

>   query :: MonadState s m => (s -> s) -> m s
>   query f = get >>= \v -> put (f v) >> return v

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
