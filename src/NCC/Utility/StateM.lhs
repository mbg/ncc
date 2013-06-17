{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains the implementation for a variation of the state monad transformer which wraps 
its result value into the Maybe type.

> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

> module Utility.StateM (
>   StateMT,
>   StateM,
>   lift,
>   put,
>   get,
>   gets,
>   state,
>   modify,
>   runStateMT,
>   evalStateMT,
>   execStateMT,
>   failMT,
>   catchMT,
>   (<&>),
>   runStateM,
>   evalStateM,
>   execStateM
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import Control.Applicative
>   import Control.Monad.Identity
>   import Control.Monad.IO.Class
>   import Control.Monad.State.Class
>   import Control.Monad.Trans.Class

    {----------------------------------------------------------------------}
    {-- StateMT Monad                                                     -}
    {----------------------------------------------------------------------}

    The state maybe transformer monad represents computations with state
    that result in a monadic computation that results in a new state and
    maybe a result.
    
>   newtype StateMT s m a = SM (s -> m (Maybe a, s))

>   instance (Monad m) => Monad (StateMT s m) where
>       return x     = SM (\s -> return (Just x, s))
>       (SM m) >>= f = SM (\s -> do 
>                       (r, s') <- m s 
>                       case r of
>                           Nothing  -> return (Nothing, s')
>                           (Just x) -> let (SM m') = f x in m' s')

    StateMT is a monad transformer and therefore we'll make it an instance of
    the MonadTrans type class which allows us to lift functions in the 
    underlying monad into the StateMT monad.

>   instance MonadTrans (StateMT s) where
>       lift f = SM (\s -> f >>= \a -> return (Just a,s))

    Since StateMT is a variation of the state monad, we will also make it
    an instance of the MonadState type class which declares the get, put and
    state functions.

>   instance (Monad m) => MonadState s (StateMT s m) where
>       get     = SM (\s -> return (Just s, s))
>       put s   = SM (\_ -> return (Just (), s))
>       state f = SM (\s -> let (r, s') = f s in return (Just r, s'))

>   runStateMT :: (Monad m) => StateMT s m a -> s -> m (Maybe a, s)
>   runStateMT (SM m) s = m s   

>   evalStateMT :: (Monad m) => StateMT s m a -> s -> m (Maybe a)
>   evalStateMT m s = fst `liftM` runStateMT m s

>   execStateMT :: (Monad m) => StateMT s m a -> s -> m s
>   execStateMT m s = snd `liftM` runStateMT m s

>   failMT :: (Monad m) => StateMT s m a
>   failMT = SM (\s -> return (Nothing, s))

>   catchMT :: (Monad m) => StateMT s m a -> StateMT s m a -> StateMT s m a
>   catchMT (SM m) (SM h) = SM (\s -> do
>       (r,s') <- m s
>       case r of
>           (Just x) -> return (r,s')
>           Nothing  -> h s')

>   (<&>) :: Monad m => StateMT s m a -> StateMT s m [a] -> StateMT s m [a]
>   (SM m) <&> (SM m') = SM (\s -> do
>       (r,s')    <- m s
>       (rs, s'') <- m' s'
>       return ((:) <$> r <*> rs,s''))

    

>   instance (Functor f) => Functor (StateMT s f) where
>       fmap f (SM m) = SM $ \s -> 
>           fmap (\(r,s') -> (fmap f r,s')) (m s)  


>   instance (MonadIO m) => MonadIO (StateMT s m) where
>       liftIO = lift . liftIO
    
    {----------------------------------------------------------------------}
    {-- StateM Monad                                                      -}
    {----------------------------------------------------------------------}

    StateM is a specialisation of the StateMT monad transformer which runs
    top of the Identity monad in the same fashion the State monad is just
    a specialisation of the StateT monad transformer on top of the Identity
    monad.
    
>   type StateM s = StateMT s Identity

>   runStateM :: StateM s a -> s -> (Maybe a, s)
>   runStateM (SM m) s = runIdentity (m s)

>   evalStateM :: StateM s a -> s -> Maybe a
>   evalStateM m = fst . runStateM m

>   execStateM :: StateM s a -> s -> s
>   execStateM m = snd . runStateM m

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
