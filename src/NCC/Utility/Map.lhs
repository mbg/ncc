{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Utility.Map (
>   mapFoldrM,
>   mapFoldrM_
> ) where

>   import qualified Data.Map as M

>   mapFoldrM :: Monad m => (k -> a -> b -> m b) -> b -> M.Map k a -> m b
>   mapFoldrM f z xs = M.foldlWithKey f' return xs z
>       where
>           f' m k a z = f k a z >>= m

>   mapFoldrM_ :: Monad m => (k -> a -> m b) -> M.Map k a -> m ()
>   mapFoldrM_ f xs = M.foldlWithKey f' return xs ()
>       where
>           f' m k a _ = f k a >>= \_ -> m ()