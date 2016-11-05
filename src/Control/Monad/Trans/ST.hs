{-# LANGUAGE MagicHash, UnboxedTuples, RankNTypes, MultiParamTypeClasses,
    FlexibleInstances, TypeFamilies, RecursiveDo, DeriveFunctor,
    UndecidableInstances #-}
{- |
   Module      :  Control.Monad.Trans
   Copyright   :  Josef Svenningsson 2008-2010
                  (c) The University of Glasgow, 1994-2000
   License     :  BSD
   Maintainer  :  mckean.kylej@gmail.com
   Stability   :  experimental
   Portability :  non-portable (GHC Extensions)
   This library provides a monad transformer version of the ST monad.
   Warning! This monad transformer should not be used with monads that
   can contain multiple answers, like the list monad. The reason is that
   the will be duplicated across the different answers and this cause
   Bad Things to happen (such as loss of referential transparency). Safe
   monads include the monads State, Reader, Writer, Maybe and
   combinations of their corresponding monad transformers.
-}
module Control.Monad.Trans.ST(
      -- * The ST Monad Transformer
      STT,
      runSTT,
      fixSTT,
      -- * Mutable references
      newSTTRef,
      readSTTRef,
      writeSTTRef,
      -- -- * Mutable arrays
      -- newSTTArray,
      -- readSTTArray,
      -- writeSTTArray,
      -- freezeSTTArray,
      -- thawSTTArray,
      -- runSTTArray,
      -- * Unsafe Operations
      unsafeIOToSTT,
      unsafeSTToIO
      )where

import GHC.Base
import GHC.STRef
import GHC.Arr (Ix(..), safeRangeSize, safeIndex,
                Array(..), STArray(..), arrEleBottom)

import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Primitive
import Control.Applicative

import Data.IORef
import Unsafe.Coerce
import System.IO.Unsafe

data STTRet s a = STTRet (State# s) a deriving (Functor)

-- | 'STT' is the monad transformer providing polymorphic updateable references
newtype STT s m a = STT { unSTT :: State# s -> m (STTRet s a) }

-- | Executes a computation in the 'STT' monad transformer
{-# NOINLINE runSTT #-}
runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT m = let (STT f) = m
 -- the parenthesis is needed because of a bug in GHC's parser
          in do (STTRet st a) <- ( f realWorld# )
                return a

-- | Allow the result of a state transformer computation to be used (lazily) inside the computation.
--   Note that if f is strict, fixSTT f = _|_.
fixSTT :: (MonadFix m) => (a -> STT s m a) -> STT s m a
fixSTT k = STT $ \ s -> do
    rec ans@(STTRet _ r) <- unSTT (k r) s
    return ans

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

-- | Create a new reference
newSTTRef :: Monad m => a -> STT s m (STRef s a)
newSTTRef init = STT $ \st1 ->
    case newMutVar# init st1 of
      (# st2, var #) -> return (STTRet st2 (STRef var))

-- | Reads the value of a reference
readSTTRef :: Monad m => STRef s a -> STT s m a
readSTTRef (STRef var) = STT $ \st1 ->
    case readMutVar# var st1 of
      (# st2, a #) -> return (STTRet st2 a)

-- | Modifies the value of a reference
writeSTTRef :: Monad m => STRef s a -> a -> STT s m ()
writeSTTRef (STRef var) a = STT $ \st1 ->
    case writeMutVar# var a st1 of
      st2 -> return (STTRet st2 ())

-- | Creates a new mutable array
newSTArray :: (Ix i, Monad m) => (i,i) -> e -> STT s m (STArray s i e)
newSTArray (l,u) init = STT $ \s1# ->
    case safeRangeSize (l,u)            of { n@(I# n#) ->
    case newArray# n# init s1#          of { (# s2#, marr# #) ->
    return (STTRet s2# (STArray l u n marr#)) }}

-- | Retrieves an element from the array
readSTArray :: (Ix i, Monad m) => STArray s i e -> i -> STT s m e
readSTArray marr@(STArray l u n _) i =
    unsafeReadSTArray marr (safeIndex (l,u) n i)

unsafeReadSTArray :: (Ix i, Monad m) => STArray s i e -> Int -> STT s m e
unsafeReadSTArray (STArray _ _ _ marr#) (I# i#)
    = STT $ \s1# -> case readArray# marr# i# s1# of
                      (# s2#, e #) -> return (STTRet s2# e)

-- | Modifies an element in the array
writeSTArray :: (Ix i, Monad m) => STArray s i e -> i -> e -> STT s m ()
writeSTArray marr@(STArray l u n _) i e =
    unsafeWriteSTArray marr (safeIndex (l,u) n i) e

unsafeWriteSTArray :: (Ix i, Monad m) => STArray s i e -> Int -> e -> STT s m ()
unsafeWriteSTArray (STArray _ _ _ marr#) (I# i#) e = STT $ \s1# ->
    case writeArray# marr# i# e s1# of
        s2# -> return (STTRet s2# ())

-- | Copy a mutable array and turn it into an immutable array
freezeSTArray :: (Ix i,Monad m) => STArray s i e -> STT s m (Array i e)
freezeSTArray (STArray l u n@(I# n#) marr#) = STT $ \s1# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr'# #) ->
    let copy i# s3# | isTrue# (i# ==# n#) = s3#
                    | otherwise           =
            case readArray# marr# i# s3# of { (# s4#, e #) ->
            case writeArray# marr'# i# e s4# of { s5# ->
            copy (i# +# 1#) s5# }} in
    case copy 0# s2#                    of { s3# ->
    case unsafeFreezeArray# marr'# s3#  of { (# s4#, arr# #) ->
    return (STTRet s4# (Array l u n arr# )) }}}

unsafeFreezeSTArray :: (Ix i, Monad m) => STArray s i e -> STT s m (Array i e)
unsafeFreezeSTArray (STArray l u n marr#) = STT $ \s1# ->
    case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
    return (STTRet s2# (Array l u n arr# )) }

-- | Copy an immutable array and turn it into a mutable array
thawSTArray :: (Ix i, Monad m) => Array i e -> STT s m (STArray s i e)
thawSTArray (Array l u n@(I# n#) arr#) = STT $ \s1# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    let copy i# s3# | isTrue# (i# ==# n#) = s3#
                    | otherwise           =
            case indexArray# arr# i#    of { (# e #) ->
            case writeArray# marr# i# e s3# of { s4# ->
            copy (i# +# 1#) s4# }} in
    case copy 0# s2#                    of { s3# ->
    return (STTRet s3# (STArray l u n marr# )) }}

unsafeThawSTArray :: (Ix i, Monad m) => Array i e -> STT s m (STArray s i e)
unsafeThawSTArray (Array l u n arr#) = STT $ \s1# ->
    case unsafeThawArray# arr# s1#      of { (# s2#, marr# #) ->
    return (STTRet s2# (STArray l u n marr# )) }

-- | A safe way to create and work with a mutable array before returning an
-- immutable array for later perusal.  This function avoids copying
-- the array before returning it.
runSTArray :: (Ix i, Monad m)
           => (forall s . STT s m (STArray s i e))
           -> m (Array i e)
runSTArray st = runSTT (st >>= unsafeFreezeSTArray)


{-# NOINLINE unsafeIOToSTT #-}
unsafeIOToSTT :: (Monad m) => IO a -> STT s m a
unsafeIOToSTT m = return $! unsafePerformIO m

unsafeSTToIO :: STT s IO a -> IO a
unsafeSTToIO m = runSTT $ unsafeCoerce m
