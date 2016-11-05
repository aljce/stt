{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}

{-# OPTIONS_HADDOCK not-home #-}

module Control.Monad.Trans.ST.Internal 
  ( STT(..)
  , STTRet(..)
  , fixSTT
  ) where

import GHC.Base

import Control.Monad.Trans
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Fix
import Control.Monad.Primitive

data STTRet s a = STTRet (State# s) a deriving (Functor)

-- | 'STT' is the monad transformer providing polymorphic updateable references
newtype STT s m a = STT { unSTT :: State# s -> m (STTRet s a) }

instance Functor m => Functor (STT s m) where
  fmap f (STT g) = STT $ \s# -> (fmap . fmap) f (g s#)

instance (Monad m, Functor m) => Applicative (STT s m) where
  pure a = STT $ \s# -> return (STTRet s# a)
  (STT m) <*> (STT n) = STT $ \s1 -> do
    (STTRet s2 f) <- m s1
    (STTRet s3 x) <- n s2
    return (STTRet s3 (f x))

instance Monad m => Monad (STT s m) where
  return a = STT $ \st -> return (STTRet st a)
  STT m >>= k = STT $ \st ->
    do STTRet new_st a <- m st
       unSTT (k a) new_st
  fail msg = lift (fail msg)

instance MonadTrans (STT s) where
  lift m = STT $ \st ->
   do a <- m
      return (STTRet st a)

instance (MonadFix m) => MonadFix (STT s m) where
  mfix = fixSTT

instance (Monad m) => PrimMonad (STT s m) where
  type PrimState (STT s m) = s
  primitive f = STT $ \s1 -> case f s1 of
    (# s2, a #) -> return (STTRet s2 a)

instance MonadError e m => MonadError e (STT s m) where
  throwError e = lift (throwError e)
  catchError (STT m) f = STT $ \st -> catchError (m st) (\e -> unSTT (f e) st)

instance MonadReader r m => MonadReader r (STT s m) where
  ask = lift ask
  local f (STT m) = STT $ \st -> local f (m st)

instance MonadState s m => MonadState s (STT s' m) where
  get = lift get
  put s = lift (put s)

instance MonadWriter w m => MonadWriter w (STT s m) where
  tell w = lift (tell w)
  listen (STT m)= STT $ \st1 -> do
    (STTRet st2 a, w) <- listen (m st1)
    return (STTRet st2 (a,w))
  pass (STT m) = STT $ \st1 -> pass (do (STTRet st2 (a,f)) <- m st1
                                        return (STTRet st2 a, f))

-- | Allow the result of a state transformer computation to be used (lazily) inside the computation.
--   Note that if f is strict, fixSTT f = _|_.
fixSTT :: (MonadFix m) => (a -> STT s m a) -> STT s m a
fixSTT k = STT $ \ s -> do
    rec ans@(STTRet _ r) <- unSTT (k r) s
    return ans

